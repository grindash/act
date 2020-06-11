#' Multiple plot with ggplot2
#' use "mfrow" from par() with ggplot
#' @param plot ggplot input
#'
#' @export
#'
#' @import grid
ggplot_print <- function(plot) {
  plot(0,0, type = "n", axes = F, ann = F)
  xpos <- act::plot_number("x") / par("mfrow")[2]
  ypos <- act::plot_number("y") / par("mfrow")[1]

  vp <- grid::viewport(height = grid::unit(1 / par("mfrow")[1], "npc"),
                       width = grid::unit(1 / par("mfrow")[2], "npc"),
                       just= c("right","bottom"),
                       y = 1 - ypos, x = xpos)

  print(plot, vp = vp)
}
