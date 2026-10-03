# core/hardware.R - hardware detection and the hardware-profile caps.
#
# Two profiles: "locked16" (CPU-only, ~16 GB locked-down PC) and "laptop" (8 GB GPU, ~24 GB RAM). The profile
# is auto-detected and can be forced with option sr.profile / env SR_PROFILE. Detection runs one local
# process (nvidia-smi) when it is installed; nothing here touches the network.

SR_PROFILES <- c("locked16", "laptop")

# Session cache of the detected hardware (an environment so it can be filled from inside functions).
.sr_hw_cache <- new.env(parent = emptyenv())

# GPU name and VRAM (MB) from nvidia-smi, or NA / NA when it is missing, fails or prints something odd.
# With several GPUs the first line is used.
.sr_hw_gpu <- function() {
  na <- list(gpu_name = NA_character_, gpu_vram_mb = NA_real_)
  exe <- Sys.which("nvidia-smi")
  if (!nzchar(exe)) return(na)
  tryCatch({
    r <- processx::run(unname(exe), c("--query-gpu=name,memory.total", "--format=csv,noheader,nounits"),
                       timeout = 5, error_on_status = FALSE)
    if (!identical(r$status, 0L)) return(na)
    lines <- trimws(strsplit(r$stdout, "[\r\n]+")[[1]])
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(na)
    parts <- trimws(strsplit(lines[[1]], ",", fixed = TRUE)[[1]])
    if (length(parts) < 2L) return(na)
    vram <- suppressWarnings(as.numeric(parts[[length(parts)]]))
    name <- paste(parts[-length(parts)], collapse = ",")
    if (is.na(vram) || !nzchar(name)) return(na)
    list(gpu_name = name, gpu_vram_mb = vram)
  }, error = function(e) na)
}

#' Detect the hardware once per session: list(ram_gb, cores, gpu_name, gpu_vram_mb, blas, os).
#' `refresh = TRUE` re-probes.
sr_hw_detect <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(.sr_hw_cache$hw)) return(.sr_hw_cache$hw)
  ram <- tryCatch(as.numeric(ps::ps_system_memory()$total) / 1024^3, error = function(e) NA_real_)
  gpu <- .sr_hw_gpu()
  blas <- tryCatch(as.character(extSoftVersion()[["BLAS"]]), error = function(e) "")
  if (length(blas) != 1L || is.na(blas)) blas <- ""
  hw <- list(
    ram_gb      = ram,
    cores       = parallel::detectCores(),
    gpu_name    = gpu$gpu_name,
    gpu_vram_mb = gpu$gpu_vram_mb,
    blas        = blas,
    os          = as.character(utils::osVersion)[1L]
  )
  .sr_hw_cache$hw <- hw
  hw
}

#' The hardware profile: the override (sr.profile / SR_PROFILE, empty = none) when set, else "laptop" iff
#' the machine has an 8 GB-class GPU (>= 6000 MB VRAM) and >= 20 GB RAM, else "locked16".
sr_profile <- function(hw = sr_hw_detect()) {
  ov <- sr_opt("profile", "")
  if (!is.character(ov) || length(ov) != 1L || is.na(ov)) {
    stop("sr.profile / SR_PROFILE must be one of: ", paste(SR_PROFILES, collapse = ", "), call. = FALSE)
  }
  if (nzchar(ov)) {
    if (!ov %in% SR_PROFILES) {
      stop("sr.profile / SR_PROFILE must be one of: ", paste(SR_PROFILES, collapse = ", "),
           "; got: ", ov, call. = FALSE)
    }
    return(ov)
  }
  vram <- if (is.null(hw$gpu_vram_mb)) NA_real_ else hw$gpu_vram_mb
  ram <- if (is.null(hw$ram_gb)) NA_real_ else hw$ram_gb
  if (isTRUE(!is.na(vram) && vram >= 6000 && !is.na(ram) && ram >= 20)) "laptop" else "locked16"
}

#' The resource caps of a profile.
sr_profile_caps <- function(profile) {
  if (!is.character(profile) || length(profile) != 1L || is.na(profile) || !profile %in% SR_PROFILES) {
    stop("unknown profile; must be one of: ", paste(SR_PROFILES, collapse = ", "), call. = FALSE)
  }
  switch(profile,
    locked16 = list(workers = 3L, ram_budget_gb = 10, duckdb_memory_gb = 6, duckdb_threads = 4L,
                    model_tier = "von", gpu = FALSE),
    laptop   = list(workers = 8L, ram_budget_gb = 18, duckdb_memory_gb = 14, duckdb_threads = 12L,
                    model_tier = "clm", gpu = TRUE)
  )
}

#' A hash of the compute environment that enters content hashes: profile, BLAS library name, GPU
#' presence and the R version. Hostnames and paths are deliberately left out.
sr_hw_fingerprint <- function(profile = sr_profile(), hw = sr_hw_detect()) {
  blas <- if (is.null(hw$blas) || length(hw$blas) != 1L || is.na(hw$blas)) "" else basename(hw$blas)
  gpu <- !is.null(hw$gpu_name) && length(hw$gpu_name) == 1L && !is.na(hw$gpu_name)
  .sr_sha256(.sr_json(list(profile = profile, blas = blas, gpu = gpu, r = R.version.string)))
}
