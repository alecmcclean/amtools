#' MakePrettyNumber
#'
#' This function chooses number of decimal places to display.
#' @param dataset Dataset to investigate
#' @keywords string, capitalize
#' @export
#' @examples
#' MakePrettyNumber()
MakePrettyNumber <- function(x) {
  size <- max(abs(x), na.rm = TRUE)

  if (is.integer(x)) {
    dec <- 0

  } else {
    dec <- 0 +
      4 * (size <  1) +
      3 * (size >= 1   & size < 10) +
      2 * (size >= 10  & size < 100) +
      1 * (size >= 100 & size < 1000)

  }

  sprintf(paste0("%.", dec, "f"), x)
}
