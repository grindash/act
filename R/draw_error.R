#' Ajoute les intervals de confiance ou les barres d'erreur sur un graph.
#'
#' @param data data.frame
#' @param x character. colname of the set of data to draw
#' @param y character. colname of the set of data to draw
#' @param z character. colname of the set of data to draw
#' @param interval logical. If TRUE display the confidence interval of the y mean
#' @param errorbar logical. If TRUE display the error bar of the y mean
#'
#' @return graphique
#' @export
draw_error <- function(data, x, y, z=NA, interval=F, errorbar=F) {
  if (!(interval | errorbar)) interval <- T
  
  if (is.na(z)) {
    data$na <- 1
    z <- "na"
  }
  
  if (length(grep("lower", colnames(data))) == 0) data <- act::get_data_mean(data, x, y, z)
  
  # Confidence interval ####
  if (interval) {
    if (is.na(z)) nb_pch <- 1 else nb_pch <- nrow(table(act::unique_table(data, z)))
    
    for (ech in seq_len(nb_pch)) {
      # Calculate the confidence interval
      interval_columns <- grep(paste(c(x, "lower", y, "upper"), collapse = "|"), colnames(data))
      conf_int <- data[data[, z] == unique(data[, z])[ech], interval_columns]
      
      # Remove NaN value preventing the polygon to correctly appear
      conf_int[is.nan(conf_int[, 2]), 2] <- conf_int[is.nan(conf_int[, 2]), 3]
      conf_int[is.nan(conf_int[, 4]), 4] <- conf_int[is.nan(conf_int[, 4]), 3]
      
      # set interval colors, alpha set the color opacity
      interval_col <- rgb(t(col2rgb(palette())), alpha = ceiling(0.3 * 255), maxColorValue = 255)
      
      # draw confidence interval
      polygon_x <- c(conf_int[, x], rev(conf_int[, x]))
      polygon_y <- c(conf_int[, grep("lower", colnames(conf_int))], rev(conf_int[, grep("upper", colnames(conf_int))]))
      polygon(polygon_x, polygon_y, col = interval_col[ech], border = "black")
    }
  }
  
  # Error Bars ####
  if (errorbar) {
    save_warn <- options()$warn
    options(warn = -1)
    
    arrows(x0 = data[, x],
           x1 = data[, x],
           y0 = data[, grep("upper", colnames(data))],
           y1 = data[, grep("lower", colnames(data))],
           length = 0.05, angle = 90, code = 3)
    
    options(warn = save_warn)
  }
}