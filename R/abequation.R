#' Draw a line following an equation.
#'
#' @param x data for the equation
#' @param coefs equation coefficient
#' @param col line color
#' @param print Draw the line on a plot if TRUE. Return result if FALSE.
#' @param lg.pos Equation legend position if print is TRUE.
#'
#' @return add a line on a pre-existing plot
#' @export
abequation <- function(x, coefs = c(a = 0.69, b = -1.42, c = 1.07), print = TRUE, col = "black", lg.pos = "topright") {
  coefs <- unlist(coefs)
  eq <- ""
  for (coef in seq_len(length(coefs))-1) {
    eq <- paste(eq, sprintf("coefs[length(coefs) - %s] * x ^ %s", coef, coef), sep = " + ")
  }
  
  y <- as.numeric(eval(parse(text = eq)))
  
  if (print) {
    points(x, y, type = "l", lwd = 2, col = col)
    
    print_eq <- "y ="
    for (i in seq_len(length(coefs))) {
      print_eq <- paste0(print_eq, if (i > 1) " + " else " ", round(coefs[i], 2),  "x^", length(coefs) - i)
      print_eq <- gsub("-", "- ", print_eq, fixed = T)
      print_eq <- gsub("+ -", "-", print_eq, fixed = T)
      print_eq <- gsub("x^1", "x", print_eq, fixed = T)
      print_eq <- gsub("x^0", "", print_eq, fixed = T)
      print_eq <- gsub("  ", " ", print_eq, fixed = T)
    }
    
    legend(lg.pos, print_eq, lty = 1, lwd = 2, col = col, bty = "n")
  } else return(y)
}