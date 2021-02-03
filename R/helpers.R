# copied from shinySurvey
#' Error out on unavailable optional pkgs
#' @inheritParams requireNamespace
#' @noRd
requireNamespace2 <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop(
      paste(
        x,
        "needed for this function to work.",
        "Please install it."
      ),
      call. = FALSE
    )
  }
}

# Create path to examples
path_ex_file <- function(...) {
  args <- c("examples", list(...))
  rlang::exec(system.file, !!!args, package = "biblids", mustWork = TRUE)
}
