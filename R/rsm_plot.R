#' Response Surface Methods plot
#' Build a second order model from the variables and draw a contour plot and a perspective plot.
#'
#' @param data Data Frame
#' @param x First factor
#' @param y Second factor
#' @param z Variable
#' @param method "contour" and / or "persp", Default is both.
#' @param col_pal color palette
#'
#' @return RSM plot
#' @export
#'
#' @importFrom rsm rsm
#' @import graphics
#' @examples data("iris")
#' @examples par(mfrow = c(1, 2))
#' @examples rsm_plot(iris, "Sepal.Length", "Sepal.Width", "Petal.Length")
rsm_plot <- function(data, x, y, z, method = c("contour", "persp"), col_pal = colorRampPalette(c("#660000", "#ff0000"))(8)) {
  #Model
  data <- na.omit(data[, c(x, y, z)])
  colnames(data) <- c("x", "y", "z")
  for (col in colnames(data)) {
    if (is.character(data[, col])) {
      cat(paste0("Warning : '", get(col), "' is not numerical, treated as Date\n"))
      data[, col] <-  as.numeric(as.Date(data[, col]))
      assign(paste0(col, "_lab"), unique(as.Date(data[, col], origin = "1970-01-01")))
    } else {
      assign(paste0(col, "_lab"), round(seq(0, max(data[, col]), max(data[, col]) / 10), 2))
    }
  }
  model <- rsm::rsm(z ~ SO(x, y), data=data)

  #RSM Plot
  if (length(grep("contour", method)) > 0) {
    par(fg = "white")
    contour(model, ~ x + y, xlabs = c(x, y), main = z, image = TRUE, xaxt = "n", yaxt = "n", lwd = 2, img.col = col_pal)
    par(fg = "black")

    if (length(unique(data[, "x"])) == length(x_lab)) xlab_pos <- unique(data[, "x"]) else xlab_pos <- seq(0, max(data[, "x"]), max(data[, "x"]) / 10)
    if (length(unique(data[, "y"])) == length(y_lab)) ylab_pos <- unique(data[, "y"]) else ylab_pos <- seq(0, max(data[, "y"]), max(data[, "y"]) / 10)

    axis(side = 2, at = ylab_pos, labels = y_lab, lwd = 0, lwd.ticks = 1)
    axis(side = 1, at = xlab_pos, labels = x_lab, lwd = 0, lwd.ticks = 1)
  }

  #3D Plot
  if (length(grep("persp", method)) > 0) {
    options(warn = -1)
    persp(model, ~x + y, phi = 20, theta = 20, xlabs = c(x, y), zlab = z, col = col_pal, contour = "color")
    options(warn = 1)
  }
}
