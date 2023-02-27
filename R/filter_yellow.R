#' Get only yellow pixels from an image.
#'
#' @param img EBImage file
#' @param thresh R and G intensity threshold (RGB), values under this threshold are not considered 
#' @param yellow_tresh threshold based on the \% of yellow pixel of each (CMY)
#' @param cyan_tresh threshold based on cyan intensities of each pixel (CMY) intensities.
#' @param display logical. Display the filtered image if TRUE.
#'
#' @return EBImage file
#' @export
#'
#' @importFrom EBImage channel rgbImage display
filter_yellow <- function(img, thresh = 0.8, yellow_tresh = 0.04, cyan_tresh = 0.3, display = F) {
  #RGB de chaque pixel
  img_size <- nrow(img) * ncol(img)
  
  rgb <- cbind(EBImage::channel(img, "r")[1:img_size],
               EBImage::channel(img, "g")[1:img_size],
               EBImage::channel(img, "b")[1:img_size])
  rgb <- data.frame(rgb)
  colnames(rgb) <- c("r", "g", "b")
  
  col_max <- max.col(rgb, ties.method = "first")  # get the dominant color of each pixel. 1 = r, 2 = g, 3 = b
  
  # Filter based on RGB
  rgb[rgb$r < thresh & rgb$g < thresh, "r"] <- 0
  rgb[rgb$r < thresh & rgb$g < thresh, "g"] <- 0
  rgb[rgb$r < thresh & rgb$g < thresh, "b"] <- 0
  
  #if g > r the pixel comes from a leaves
  rgb[rgb$r < rgb$g, "r"] <- 0
  rgb[rgb$r < rgb$g, "g"] <- 0
  rgb[rgb$r < rgb$g, "b"] <- 0
  
  #RGB to CMY conversion
  K = 1 - do.call("pmax", rgb)  # The black key (K)
  C = (1 - rgb$r - K) / (1 - K)  # The cyan color (C)
  M = (1 - rgb$g - K) / (1 - K)  # The magenta color (M)
  Y = (1 - rgb$b - K) / (1 - K)  # The yellow color (Y)
  
  # Filter based on CMY
  y_h <- hist(Y, breaks = 50, plot = F)
  yellow_tresh <- max(y_h$breaks[which(y_h$counts > yellow_tresh * img_size)])
  rgb[Y < yellow_tresh, "r"] <- 0
  rgb[Y < yellow_tresh, "g"] <- 0
  rgb[Y < yellow_tresh, "b"] <- 0
  C[Y < yellow_tresh] <- 0
  M[Y < yellow_tresh] <- 0
  
  # rgb[C > cyan_tresh, "r"] <- 0
  # rgb[C > cyan_tresh, "g"] <- 0
  # rgb[C > cyan_tresh, "b"] <- 0
  
  # Export
  r <- matrix(rgb[, "r"], ncol = ncol(img))
  g <- matrix(rgb[, "g"], ncol = ncol(img))
  b <- matrix(rgb[, "b"], ncol = ncol(img))
  img_rgb <- EBImage::rgbImage(r, g, b)
  if (display) EBImage::display(img_rgb, method = "raster")
  return(img_rgb)
}
