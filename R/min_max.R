#' Combine min and max functions
#'
#' @param data data frame
#'
#' @return c(min, max)
#' @export
#'
#' @examples data("cars")
#' @examples min_max(cars)
min_max <- function(data) {
  data <- na.omit(data)
  return(c(min(data), max(data)))
}
