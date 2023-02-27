#' Returns TRUE if its argument is a date and FALSE otherwise.
#'
#' @param x argument to test
#' @param format date format, default is \%Y-\%m-\%d
#'
#' @return logical
#' @export
is.date <- function(x, format = "%Y-%m-%d") {
  return(!all(is.na(as.Date(as.character(x), format = format))))
}