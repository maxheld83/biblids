.onLoad <- function(libname, pkgname) {
  can_cache <- requireNamespace("memoise", quietly = TRUE) &
    requireNamespace("cachem", quietly = TRUE)
  if (can_cache) {
    verb_doi <<- memoise::memoise(
      retry_doi,
      cache = getOption(
        "biblids.cache",
        default = cachem::cache_mem(max_size = 1024 * 1024^2)
      )
    )
  }
}
