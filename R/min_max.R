#' Combine min and max functions
#'
#' @param data data frame
#'
#' @return c(min, max)
#' @export
#'
#' @examples data("cars")
#' @examples min_max(cars)
min_max <- function(data, which = F) {
  data <- na.omit(data)
  data <- as.numeric(data)
  if (length(unique(data)) == 1) {
    if (is.na(unique(data))) stop("Error in act::min_max, make sure data is a numeric or date format")
  }
  
  if (which == F) return(c(min(data), max(data)))
  if (which == T) return(c(which.min(data), which.max(data)))
}
