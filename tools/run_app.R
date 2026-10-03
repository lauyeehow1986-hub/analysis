# Launch the StatRouter app: Rscript tools/run_app.R   (works from any working directory)
# Serves http://127.0.0.1:<port> only (never a public interface). The port comes from sr.port /
# SR_PORT and defaults to 7790. Does not open a browser.

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))

options(shiny.autoload.r = FALSE)
port <- suppressWarnings(as.integer(getOption("sr.port", Sys.getenv("SR_PORT", "7790"))))
if (length(port) != 1L || is.na(port)) stop("sr.port / SR_PORT must be an integer port number")
shiny::runApp(file.path(root, "app"), host = "127.0.0.1", port = port, launch.browser = FALSE)
