# copied from shinySurvey
#' Error out on unavailable optional pkgs
#' @inheritParams requireNamespace
#' @noRd
require_namespace2 <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    rlang::abort(
      paste(
        x,
        "needed for this function to work.",
        "Please install it."
      )
    )
  }
}

#' Create path to example
#' Used in example tag and tests
#' @noRd
path_ex_file <- function(...) {
  args <- c("examples", list(...))
  rlang::exec(system.file, !!!args, package = "biblids", mustWork = TRUE)
}

#' Source path_ex_files
#' @noRd
source_pef <- function(...) {
  source(path_ex_file(...))$value
}
