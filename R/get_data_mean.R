#' Calcul la moyenne et l'ecart-type pour une condition
#'
#' @param data data.frame
#' @param x character. colname of the set of data to draw
#' @param y character. colname of the set of data to draw
#' @param z character. colname of the set of data to draw. Default is NA. Can have a length of 2 colnames.
#' @param nb_col_mean number of factor
#'
#' @return data.frame
#' @export
#'
#'@importFrom Rmisc group.CI
get_data_mean <- function(data, x, y, z, nb_col_mean=0) {
  if (length(z) == 1 && is.na(z)) {
    data$na <- 1
    z <- "na"
  }
  
  col_mean <- grep(paste(x, paste(z, collapse = "|"), sep = "|"), colnames(data)[1:nb_col_mean], invert = T, value = T)
  mean_formula <- paste(y, "~", paste(col_mean, collapse = "+"), "+", x, "+", paste(z, collapse = "+"))
  
  options(warn = -1)
  data_mean <- Rmisc::group.CI(as.formula(mean_formula), data = data)
  options(warn = +1)
  
  colnames(data_mean)[grep("mean", colnames(data_mean))] <- y
  
  return(data_mean)
}