# observation dans un angle de la photo
#' Get all pixel of a picture within an angle. Origin is the photo center.
#'
#' @param img EBImage file
#' @param channel either name of a rgb channel ("r", "g" or "b") or an EBImage::channel matrix
#' @param angle_init degree. starting point of the sensor
#' @param angle_sensor degree. sensor field of view
#'
#' @return EBImage file
#' @export
#'
#' @importFrom EBImage channel
#' @import REdaS
filter_angle <- function(img, channel = "g", angle_init = 0, angle_sensor = 15) {
  img_size <- nrow(img) * ncol(img)
  center <- lapply(list(x = nrow(img) / 2, y = ncol(img) / 2), round)
  rayon <- sqrt(nrow(img) ^ 2 + ncol(img) ^ 2)  # calcule la diagonale entre le centre et le coin de l'image
  angle_init <- REdaS::deg2rad(angle_init)
  angle_sensor <- REdaS::deg2rad(angle_sensor)
  
  vect1 <- list(x = center$x + rayon * cos(-pi / 2 + angle_init), 
                y = center$y + rayon * sin(-pi / 2 + angle_init))
  vect2 <- list(x = center$x  + rayon * cos(-pi / 2 + angle_init + angle_sensor),
                y = center$y  + rayon * sin(-pi / 2 + angle_init + angle_sensor))
  
  # channel import
  if (is.character(channel)) {
    channel <- matrix(EBImage::channel(img, channel)[1:img_size], ncol = ncol(img))
  } else if (is.matrix(channel)) {
    channel <- channel
  }
  
  # Compute triangle bounding box
  minX <- round(min(c(center$x, vect1$x, vect2$x)))
  maxX <- round(max(c(center$x, vect1$x, vect2$x)))
  minY <- round(min(c(center$y, vect1$y, vect2$y)))
  maxY <- round(max(c(center$y, vect1$y, vect2$y)))
  
  # Clip against screen bounds
  minX <- max(minX, 1)
  maxX <- min(maxX, nrow(img))
  minY <- max(minY, 1)
  maxY <- min(maxY, ncol(img))
  
  # Rasterize, based on https://fgiesen.wordpress.com/2013/02/08/triangle-rasterization-in-practice/
  orient2d <- function(a, b, c) {return((b$x - a$x) * (c$y - a$y) - (b$y - a$y) * (c$x - a$x))}
  
  pt_list <- expand.grid(seq(minX, maxX), seq(minY, maxY))
  colnames(pt_list) <- c("x", "y")
  
  # Determine barycentric coordinates
  w0 <- orient2d(vect1, vect2, pt_list)
  w1 <- orient2d(vect2, center, pt_list)
  w2 <- orient2d(center, vect1, pt_list)
  pt_list <- pt_list[w0 >= 0 & w1 >= 0 & w2 >= 0, ]  # keep points within triangle
  
  # Get intensity value for pixels in the triangle 
  channel_output <- matrix(0, ncol = ncol(img), nrow = nrow(img))  # empty new channel
  
  shortest_col <- colnames(pt_list)[which.min(c(length(unique(pt_list$x)), length(unique(pt_list$y))))]  # get wich of x and y has the fewer points
  longest_col <- colnames(pt_list)[which.max(c(length(unique(pt_list$x)), length(unique(pt_list$y))))]  # get wich of x and y has the greater points
  for (coord.1 in unique(pt_list[, shortest_col])) {
    coord.2 <- pt_list[pt_list[, shortest_col] == coord.1, longest_col]
    
    #Replace values
    if (shortest_col == "x") channel_output[coord.1, coord.2] <- channel[coord.1, coord.2]
    if (shortest_col == "y") channel_output[coord.2, coord.1] <- channel[coord.2, coord.1]
  }
  
  return(channel_output)
}