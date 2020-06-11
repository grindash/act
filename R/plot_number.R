#' Return the number of the current plot.
#'
#' @param out "pos" give plot number. "x" give column number of the current plot, "y" give row number of the current plot
#'
#' @return Plot number in current window.
#' @export
#'
#' @examples plot_number()
plot_number <-  function(out="pos") {
  x <- round(1 + par("fig")[1] * par("mfrow")[2])
  y <- round((1 - par("fig")[3]) * par("mfrow")[1])
  nb <- sum(x, (y - 1) * par("mfrow")[2])

  if (out == "pos") return(nb)
  if (out == "x") return(x)
  if (out == "y") return(y)
}
