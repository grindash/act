#' Draw a line following an equation.
#'
#' @param x data for the equation
#' @param coefs equation coefficient
#' @param col line color
#'
#' @return add a line on a pre-existing plot
#' @export
abequation <- function(x, coefs = c(a = 0.69, b = -1.42, c = 1.07), col = "black") {
  coefs <- unlist(coefs)
  eq <- ""
  for (coef in seq_len(length(coefs))-1) {
    eq <- paste(eq, sprintf("coefs[length(coefs) - %s] * x ^ %s", coef, coef), sep = " + ")
  }
  as.numeric(eval(parse(text = eq)))
  points(x, as.numeric(eval(parse(text = eq))), type = "l", lwd = 2, col = col)
}