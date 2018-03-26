#' Capitalize
#'
#' This function capitalizes the first letter of a normal string
#' @param dataset Dataset to investigate
#' @keywords string, capitalize
#' @export
#' @examples
#' Capitalize()
Capitalize <- function(string) {
  paste0(
    substr(toupper(substr(string, 1, 1))),
    substr(string, 2, nchar(string))
  )
}
