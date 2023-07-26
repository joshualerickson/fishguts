#' Capitalize
#'
#' @param x A string
#'
#' @return A string capitalized.
#' @noRd
capit <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
