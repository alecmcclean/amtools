#' CountNum
#'
#' This function counts the number with same syntzx as mean, max, min.
#' @param dataset Dataset to investigate
#' @keywords string, capitalize
#' @export
#' @examples
#' CountNum()
CountNum <- function(x, na.rm) {
  as.integer(sum(!is.na(x)))
}
