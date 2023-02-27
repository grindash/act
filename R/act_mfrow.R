#' Calculate the size of the graphic windows base on the data set.
#'
#' @param df data frame
#' @param col column of the parameters analysed
#' @param range_nb gamme de taille de fenetre a tester
#'
#' @return c(nrow,ncol)
#' @export
act_mfrow <- function(df, col="all", range_nb=2:10) {
  if (length(df) == 1) {
    nb <- df
  } else {
    df <- as.data.frame(df)
    if (length(col) == 1) {
      if (col == "all") col <- 1:ncol(df)
      nb <- length(unique(df[, col]))
    }else{
      nb <- nrow(act::unique_table(df[, col]))
    }
  }
  
  if (nb == 1) {
    return(c(1, 1))
  } else {
    is_prime <- function(x) {
      div <- 2:ceiling(sqrt(x))
      !any(x %% div == 0)
    }
    
    if (is_prime(nb)) {
      mfrow <- c(round(sqrt(nb)), ceiling(sqrt(nb)))
    } else {
      close_nb <- sapply(range_nb, function(x) {if (nb %% x == 0) abs((nb / x) - x) else NA})
      nb_col <- range_nb[which.min(close_nb)]
      nb_row <- nb / nb_col
      mfrow <- c(min(nb_row, nb_col), max(nb_row, nb_col))
      
      if ((mfrow[2] / mfrow[1]) > 3) mfrow <- c(round(sqrt(nb)), ceiling(sqrt(nb)))
    }
    return(mfrow)
  }
}
