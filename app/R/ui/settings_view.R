# ui/settings_view.R - the pure side of the Settings panel: the catalogue of session settings, validation,
# applying and resetting an option, and the table of effective values. No Shiny here.
#
# A setting is the option sr.<name> (sr_opt falls back to the env var SR_<NAME>, then to the default). Edits
# are session-only: nothing is written to disk and nothing is a project event. The Jev key is held only as
# options(sr.jev_key); no function here returns or prints its value.

# The settings the panel edits, in the order of the panel: name, label, editor kind and the default the core's
# own sr_opt() call uses (a test compares them). `default` is a list column (the kinds differ).
.sr_set_catalogue <- function() {
  rows <- list(
    list("mode", "Mode", "choice", "airgap"),
    list("profile", "Hardware profile", "choice", ""),
    list("user", "Actor (user)", "text", NA_character_),
    list("digits", "Digits", "integer", 2),
    list("router_mask", "Mask router data", "logical", FALSE),
    list("jev_key", "Jev key", "secret", ""),
    list("jev_url", "Jev URL", "text", "https://api.typesafe.ai/v1/systemone"),
    list("jev_model", "Jev model", "text", "jev-latest"),
    list("jev_timeout", "Jev timeout (s)", "number", 20),
    list("freeze_full_max_bytes", "Freeze: largest full data copy (bytes)", "number", 2147483648),
    list("srproj_max_total", "Import: largest archive total (bytes)", "number", 68719476736),
    list("cran_snapshot", "CRAN snapshot date", "date", NA)
  )
  data.frame(name = vapply(rows, `[[`, "", 1L), label = vapply(rows, `[[`, "", 2L),
             editor = vapply(rows, `[[`, "", 3L), default = I(lapply(rows, `[[`, 4L)),
             stringsAsFactors = FALSE)
}

.sr_set_names <- function() .sr_set_catalogue()$name

# One scalar string from `value`, trimmed; NA when it is not one.
.sr_set_str <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return(NA_character_)
  trimws(value)
}

# One finite number from `value`, or NA.
.sr_set_num <- function(value) {
  if (is.character(value)) value <- trimws(value)
  x <- suppressWarnings(as.numeric(value))
  if (length(x) != 1L || !is.finite(x)) return(NA_real_)
  x
}

#' The typed value of setting `name` for the text or number `value`; stops with the message of the rule it
#' breaks. The message never contains `value` (it may be the Jev key).
.sr_set_coerce <- function(name, value) {
  nonempty <- function() {
    s <- .sr_set_str(value)
    if (is.na(s) || !nzchar(s)) stop(name, " must not be empty", call. = FALSE)
    s
  }
  bytes_msg <- paste0(name, " must be a finite number of bytes, 0 or more")
  switch(name,
    mode = {
      s <- .sr_set_str(value)
      if (!isTRUE(s %in% c("airgap", "connected"))) stop("mode must be airgap or connected", call. = FALSE)
      s
    },
    profile = {
      s <- .sr_set_str(value)
      if (!isTRUE(s %in% c("auto", SR_PROFILES))) {
        stop("profile must be auto, ", paste(SR_PROFILES, collapse = " or "), call. = FALSE)
      }
      s
    },
    user = {
      s <- .sr_set_str(value)
      if (is.na(s) || !nzchar(s)) stop("the actor must not be empty (use Reset)", call. = FALSE)
      s
    },
    digits = {
      x <- .sr_set_num(value)
      if (is.na(x) || x < 0 || x != round(x) || x > .Machine$integer.max) {
        stop("digits must be a whole number, 0 or more", call. = FALSE)
      }
      as.integer(x)
    },
    router_mask = {
      if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
      s <- tolower(.sr_set_str(value))
      if (isTRUE(s %in% c("on", "true"))) return(TRUE)
      if (isTRUE(s %in% c("off", "false"))) return(FALSE)
      stop("router_mask must be on or off", call. = FALSE)
    },
    jev_key = nonempty(),
    jev_url = nonempty(),
    jev_model = nonempty(),
    jev_timeout = {
      x <- .sr_set_num(value)
      if (is.na(x) || x <= 0) stop("jev_timeout must be a positive number of seconds", call. = FALSE)
      x
    },
    freeze_full_max_bytes = ,
    srproj_max_total = {
      x <- .sr_set_num(value)
      if (is.na(x) || x < 0) stop(bytes_msg, call. = FALSE)
      x
    },
    cran_snapshot = {
      s <- .sr_set_str(value)
      d <- if (is.na(s)) as.Date(NA) else as.Date(s, "%Y-%m-%d")
      if (is.na(d) || !identical(format(d), s)) stop("cran_snapshot must be a date YYYY-MM-DD", call. = FALSE)
      s
    },
    stop("unknown setting: ", name, call. = FALSE)
  )
}

# Set option sr.<name> to `v` (NULL removes it).
.sr_set_option <- function(name, v) {
  options(stats::setNames(list(v), paste0("sr.", name)))
  invisible(NULL)
}

#' Validate and set setting `name`. For mode and profile the core's own check (sr_mode, sr_profile) runs
#' on the new value; on any error the previous option is restored and the message re-thrown. A profile of
#' "auto" resets the option.
.sr_set_apply <- function(name, value) {
  v <- .sr_set_coerce(name, value)
  old <- getOption(paste0("sr.", name))
  tryCatch({
    .sr_set_option(name, if (identical(name, "profile") && identical(v, "auto")) NULL else v)
    if (identical(name, "mode")) sr_mode()
    if (identical(name, "profile")) sr_profile()
  }, error = function(e) {
    .sr_set_option(name, old)
    stop(conditionMessage(e), call. = FALSE)
  })
  invisible(v)
}

#' Reset setting `name` (env var or default applies again). Resetting mode also clears the Jev key.
.sr_set_reset <- function(name) {
  .sr_set_option(name, NULL)
  if (identical(name, "mode")) .sr_set_option("jev_key", NULL)
  invisible(NULL)
}

#' The settings table: setting, value, source. `edited` names the settings set in this session. The Jev key is
#' shown as "set" or "not set", never its value.
.sr_set_table <- function(edited = character()) {
  cat <- .sr_set_catalogue()
  rows <- lapply(seq_len(nrow(cat)), function(i) {
    nm <- cat$name[i]
    def <- cat$default[[i]]
    src <- sr_opt_source(nm)
    if (identical(nm, "jev_key")) {
      isset <- nzchar(as.character(sr_opt("jev_key", ""))[1L])
      val <- if (isset) "set" else "not set"
      if (!isset) src <- "default"
    } else if (identical(nm, "profile")) {
      val <- tryCatch(sr_profile(), error = function(e) paste0("invalid: ", conditionMessage(e)))
      if (identical(src, "default")) src <- "auto-detected"
    } else if (identical(nm, "user")) {
      val <- format(sr_opt("user", Sys.info()[["user"]]))
    } else {
      val <- paste(format(sr_opt(nm, def)), collapse = " ")
    }
    if (nm %in% edited) src <- "edited here"
    data.frame(setting = nm, value = val, source = src, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' The note shown under a setting that has a side effect on later runs, or NULL.
.sr_set_note <- function(name) {
  switch(name,
    profile = paste("Changing the profile changes the compute fingerprint (sr_hw_fingerprint), so later runs",
                    "refit rather than reuse cached fits."),
    cran_snapshot = paste("The CRAN snapshot date enters the library snapshot hash (sr_libsnap): changing it",
                          "makes later runs show \"library changed\" against the lock, and after a freeze it can",
                          "call for a re-baseline."),
    NULL)
}
