#' Create an ordered dataframe with all unique combination of a data set
#'
#' @param data data frame
#' @param factors Either column numbers or column names
#'
#' @return Data frame with all combinations
#' @export
#'
#' @examples data("CO2")
#' @examples unique_table(CO2, c(1:3))
unique_table <- function(data, factors){
  if (typeof(factors) == "double") factors <- colnames(data)[factors]
  table <- data.frame(unique(data[, factors]))

  for (i in rev(seq_len(length(factors)))) {
    table <- table[order(table[, i]), ]
  }
  table <- data.frame(table)
  colnames(table) <- factors
  # table[] <- lapply(table, as.character)

  return(table)
}
