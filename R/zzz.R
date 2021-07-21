.onLoad <- function(libname, pkgname) {
  can_cache <- requireNamespace("memoise", quietly = TRUE) &
    requireNamespace("cachem", quietly = TRUE)
  if (can_cache) {
    verb_doi <<- memoise::memoise(retry_doi)
  }
  if (requireNamespace("shiny", quietly = TRUE)) {
    options(wama.default.app = doiEntryApp())
  }
}
