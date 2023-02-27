#' Response Surface Methods plot
#' Build a second order model from the variables and draw a contour plot and a perspective plot.
#'
#' @param data Data Frame
#' @param x First factor
#' @param y Second factor
#' @param z Variable
#' @param method "contour" and / or "persp", Default is both.
#' @param col_pal color palette. default is 8 shades of red
#' @param flip echange l'axe x et y, permet d econtourner certains bugs
#' @param ... arguments for par()
#'
#' @return RSM plot
#' @export
#'
#' @import rsm
#' @import graphics
#' 
#' @examples 
#' data("iris")
#' par(mfrow = c(1, 2))
#' rsm_plot(iris, "Sepal.Length", "Sepal.Width", "Petal.Length")
rsm_plot <- function(data, x, y, z, method = c("contour", "persp"), col_pal = NA, flip = F, ...) {
  if (is.na(col_pal)) col_pal <- colorRampPalette(c("#660000", "#ff0000"))(8)
  
  par(mfrow = c(1, length(method)), ...)
  
  #Model
  data <- na.omit(data[, c(x, y, z)])
  colnames(data) <- c("x", "y", "z")
  x_lab <- NULL
  y_lab <- NULL
  for (col in colnames(data)[1]) {
    if (is.numeric(data[, col])) {
      assign(paste0(col, "_lab"), round(seq(0, max(data[, col]), max(data[, col]) / 10), 2))
    } else {
      # warning(paste0("'", get(col), "' is not numerical, treated as 'Date'"))
      data[, col] <-  as.numeric(as.Date(data[, col]))
      assign(paste0(col, "_lab"), unique(as.Date(data[, col], origin = "1970-01-01")))
    }
  }
  model <- rsm::rsm(z ~ SO(x, y), data = data)

  #RSM Plot
  if (length(grep("contour", method)) > 0) {
    par(fg = "white")
    if (flip) {
      graphics::contour(model, ~ y + x, xlabs = c(x, y), main = z, image = TRUE, xaxt = "n", yaxt = "n", lwd = 2, img.col = col_pal)
    } else {
      graphics::contour(model, ~ x + y, xlabs = c(x, y), main = z, image = TRUE, xaxt = "n", yaxt = "n", lwd = 2, img.col = col_pal)
    }
    par(fg = "black")

    if (length(unique(data[, "x"])) == length(x_lab)) xlab_pos <- unique(data[, "x"]) else xlab_pos <- seq(0, max(data[, "x"]), max(data[, "x"]) / 10)
    if (length(unique(data[, "y"])) == length(y_lab)) ylab_pos <- unique(data[, "y"]) else ylab_pos <- seq(0, max(data[, "y"]), max(data[, "y"]) / 10)

    graphics::axis(side = if (flip) 2 else 1, at = xlab_pos, labels = x_lab, lwd = 0, lwd.ticks = 1)
    graphics::axis(side = if (flip) 1 else 2, at = ylab_pos, labels = y_lab, lwd = 0, lwd.ticks = 1)
  }

  #3D Plot
  if (length(grep("persp", method)) > 0) {
    options(warn = -1)
    if (flip) {
      graphics::persp(model, ~ y + x, phi = 20, theta = 20, xlabs = c(x, y), zlab = z, col = col_pal, contour = "color")
    } else {
      graphics::persp(model, ~ x + y, phi = 20, theta = 20, xlabs = c(x, y), zlab = z, col = col_pal, contour = "color")
    }
    options(warn = 1)
  }
}

