#' Calculate the size of the graphic windows base on the data set.
#'
#' @param df data frame
#' @param col column of the parameters analysed
#'
#' @return c(nrow,ncol)
#' @export
act_mfrow <- function(df, col="all") {
  if (length(df) == 1) {
    if (df == 3) mfrow <- c(1, 3) else mfrow <- c(round(sqrt(df)), ceiling(sqrt(df)))
  } else {
    df <- as.data.frame(df)
    if (length(col) == 1) {
      if (col == "all") col <- 1:ncol(df)
      nb <- length(unique(df[, col]))
    }else{
      nb <- nrow(unique(df[, col]))
    }
    if (nb == 3) mfrow <- c(1, 3) else mfrow <- c(round(sqrt(nb)), ceiling(sqrt(nb)))
  }
  return(mfrow)
}
