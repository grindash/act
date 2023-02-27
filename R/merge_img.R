#' Merge multiple pictures into one
#'
#' @param img_pattern #name pattern shared by the images
#' @param path path to pictures
#'
#' @return png
#' @export
#'
#' @import graphics
#' @import png
merge_img <- function(img_pattern, path="~/") {
  img_list <- list.files(dirname(path), paste0(img_pattern, ".*?png"), recursive = T, full.names = T)
  img_list <- img_list[grep("merge", img_list, invert = T)]

  opt <- act::default_opt()
  opt$png_output <- T
  mfrow <- if (length(img_list) == 3) c(1, 3) else act::act_mfrow(img_list)
  par(mfrow = mfrow, mar = rep(0,4))
  act::make_output(file.path(dirname(img_list[1]), paste0("merge_", img_pattern, ".png")))

  for (i in img_list) {
    img <- png::readPNG(i)
    plot(NA, xlim = 0:1, ylim = 0:1, xaxt = "n", yaxt = "n", bty = "n")
    rasterImage(img,0,0,1,1)
  }

  graphics.off()
}
