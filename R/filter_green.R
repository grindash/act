#' Get only green pixels from an image.
#'
#' @param img EBImage file
#' @param seuil_rg threshold for red vs green
#' @param seuil_br threshold for blue vs red
#' @param seuil_bg threshold for blue vs green
#' @param seuil_g threshold for green
#' @param output character. "img" return a EBImage::rgbImage object. "lai" return the leaf area in the picture (green pixel number / total pixel)
#' @param display if TRUE, print the plot
#' @param ... filter_angle arguments
#'
#' @return EBImage file
#' @export
#'
#' @importFrom EBImage rgbImage channel display
filter_green <- function(img, seuil_rg = 0.82, seuil_br = 0.9, seuil_bg = 0.75, seuil_g = 0, output = "both", display = T, ...) {
  # Get rgb of each pixels
  img_size <- nrow(img) * ncol(img)
  rgb <- cbind(EBImage::channel(img, "r")[1:img_size],
               EBImage::channel(img, "g")[1:img_size],
               EBImage::channel(img, "b")[1:img_size])
  rgb <- data.frame(rgb)
  colnames(rgb) <- c("r", "g", "b")
  
  # Prevent division by zero
  rgb[which(rgb$r == 0), ] <- 10 ^ -10
  rgb[which(rgb$g == 0), ] <- 10 ^ -10
  rgb[which(rgb$b == 0), ] <- 10 ^ -10
  
  # Remove specific pixels
  rgb[(rgb$r == 1 & rgb$g == 1 & rgb$b == 1) , "g"] <- 0  # remove white pixels
  rgb[rgb$r / rgb$g > seuil_rg, "g"] <- 0
  # rgb[rgb$b / rgb$r > seuil_br & rgb$b / rgb$g > seuil_bg, "g"] <- 0  # remove grey pixels
  rgb[rgb$b / rgb$r > seuil_br, "g"] <- 0
  rgb[rgb$b / rgb$g > seuil_bg, "g"] <- 0
  green_percent <- rgb$g / (rgb$r + rgb$b)
  
  # Remove non-green pixels
  # col_max <- max.col(rgb, ties.method="first")  # get the dominant color of each pixel. 1 = r, 2 = g, 3 = b
  # rgb[!(col_max == 2), "g"] <- 0
  rgb[!((rgb$g >= rgb$r) & (rgb$g >= rgb$b) & (rgb$g > 0)),] <- 0
  rgb$r <- 0
  rgb$b <- 0
  rgb[rgb$g < seuil_g, "g"] <- 0
  # rgb[green_percent <= seuil_g, "g"] <- 0
  
  r <- matrix(rgb[, "r"], ncol = ncol(img))
  g <- matrix(rgb[, "g"], ncol = ncol(img))
  b <- matrix(rgb[, "b"], ncol = ncol(img))
  
  # Angle filter
  if (length(list(...)) > 0) g <- filter_angle(img = img, channel = g, ...)
  
  # Return
  output_img <- EBImage::rgbImage(r, g, b)
  lai <- length(g[g > 0]) / img_size
  if (display) EBImage::display(output_img, method = "raster")
  if (output == "img") return(output_img)
  if (output == "lai") return(lai)
  if (output == "both") return(list(lai = lai, img = output_img))
}
