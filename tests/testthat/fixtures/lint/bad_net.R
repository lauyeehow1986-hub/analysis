# synthetic fixture: network calls (never sourced)
f <- function() {
  download.file("a", "b")
  curl::curl_fetch_memory("a")
  reticulate::py_available(initialize = TRUE)
  reticulate::py_available()
  x <- "download.file"
  url <- 1
}
