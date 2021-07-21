.onLoad <- function(libname, pkgname) {
  can_cache <- requireNamespace("memoise", quietly = TRUE) &
    requireNamespace("cachem", quietly = TRUE)
  if (can_cache) {
    verb_doi <<- memoise::memoise(retry_doi)
  }
  if (require("shiny")) {
    options(wama.default.app = doiEntryApp())
  }
}
