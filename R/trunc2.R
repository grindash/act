#' Truncate number after n decimal
#'
#' @param x value to round
#' @param n decimal number
#'
#' @return numeric
#' @export
#'
#' @examples trunc2(1.23456, 2)
trunc2 <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ (n * 2)
  z <- z + 1
  z <- trunc(z / 10 ^ n)
  z <- z / 10 ^ (n)
  return(z * posneg)
}
