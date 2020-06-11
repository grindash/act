#' Add and image on a plot and keep the ratio of a picture.
#' Credit: https://stackoverflow.com/a/56018973
#'
#' @param img picture to import
#' @param x position
#' @param y position
#' @param width picture width
#' @param decalageX change the position reference. 0 = right, 1 = left, 0.5 = center
#' @param decalageY change the position reference. 0 = bottom, 1 = top, 0.5 = center
#' @param interpolate osef
#'
#' @return draw an image
#' @export
#'
#' @import graphics
add_img <-  function(img, x=0, y=0, width=3, decalageX=0.5, decalageY=0.5, interpolate=FALSE) {
  pixel_aspect_ratio <- dim(img)[1] / dim(img)[2]
  width_in <- width / (par("usr")[2] - par("usr")[1]) * par("pin")[1] # convert width units to inches

  height_in <- width_in * pixel_aspect_ratio # height in inches
  height_unit <- height_in / par("pin")[2] * (par("usr")[4] - par("usr")[3]) # height in units

  left <- x - (width * (1 - decalageX))
  right <- x + (width * decalageX)
  bottom <- y - (height_unit * decalageY)
  top <- y + (height_unit * (1 - decalageY))

  graphics::rasterImage(img, interpolate = interpolate,
                        xleft = left, xright = right,
                        ybottom = bottom, ytop = top)
}
