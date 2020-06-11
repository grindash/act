#' ShadesOfGrey
#' Color palette with shade from white to black
#'
#' @param n Number of colors to return. Max is 100.
#'
#' @return Hex color value
#' @export
#'
#' @importFrom grDevices colorRampPalette
#' @examples shades_of_grey(3)
shades_of_grey <- function(n) {
  if (n == 1) rep <- 2 else rep <- 1
  return(rep(colorRampPalette(c("grey100", "grey0"))(n), rep))
}
