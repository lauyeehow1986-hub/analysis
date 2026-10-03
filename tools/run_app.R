# Launch the StatRouter app: Rscript tools/run_app.R   (works from any working directory)
# Serves http://127.0.0.1:<port> only (never a public interface). The port comes from sr.port /
# SR_PORT (empty counts as unset, as in sr_opt) and defaults to 7790. Does not open a browser.

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))

options(shiny.autoload.r = FALSE)
source(file.path(root, "app", "R", "core", "util.R"), encoding = "UTF-8")   # sr_opt()
port <- suppressWarnings(as.numeric(sr_opt("port", 7790)))
if (length(port) != 1L || is.na(port) || port != floor(port) || port < 1 || port > 65535) {
  stop("sr.port / SR_PORT must be a whole number from 1 to 65535", call. = FALSE)
}
shiny::runApp(file.path(root, "app"), host = "127.0.0.1", port = as.integer(port), launch.browser = FALSE)
