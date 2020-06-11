#' Calculate the size of the graphic windows base on the data set.
#'
#' @param df data frame
#' @param col column of the parameters analysed
#'
#' @return c(nrow,ncol)
#' @export
act_mfrow <- function(df, col="all") {
  df <- as.data.frame(df)
  if (col == "all") col <- 1:ncol(df)
  if (length(col) == 1) {
    mfrow <- c(round(sqrt(length(unique(df[, col])))), ceiling(sqrt(length(unique(df[, col])))))
  }else{
    mfrow <- c(round(sqrt(nrow(unique(df[, col])))), ceiling(sqrt(nrow(unique(df[, col])))))
  }
  return(mfrow)
}
