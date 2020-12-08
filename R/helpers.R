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
