#' Combine min and max functions
#'
#' @param data data frame
#' @param which logical. Determines the index of the minimum and maximum of a numeric vector.
#'
#' @return c(min, max)
#' @export
#'
#' @importFrom Rfast colMinsMaxs
#'
#' @examples 
#' data("cars")
#' min_max(cars)
#' min_max(cars$speed)
min_max <- function(data, which = F) {
  # one column
  if (is.null(dim(data))) {  
    data <- na.omit(data)
    data <- if (length(unique(as.numeric(data))) == 1 && is.na(unique(as.numeric(data)))) data else as.numeric(data)
    
    if (length(unique(data)) == 1) {
      if (is.na(unique(data))) stop("Error in act::min_max, make sure data is a numeric or date format")
    }
  
    if (which == F) output <- c(min(data), max(data))
    if (which == T) output <- c(which.min(data), which.max(data))
    
  } 
  
  # multiple columns
  else {  
    output <- Rfast::colMinsMaxs(as.matrix(data))
    colnames(output) <- colnames(data)
    if (which == T) warning("Can't give index for multiple columns, 'which' argument ignored")
  }
  
  return(output)
}
