#' Create an ordered dataframe with all unique combination of a data set
#'
#' @param data data frame
#' @param factors Either column numbers or column names
#'
#' @return Data frame with all combinations
#' @export
#'
#' @examples
#' data("CO2")
#' unique_table(CO2, c(1:3))
unique_table <- function(data, factors=NULL){
  data <- data.frame(data)
  if (is.null(factors[1])) factors <- colnames(data)
  if (typeof(factors) == "double") factors <- colnames(data)[factors]
  table <- data.frame(unique(data[, factors]))

  for (i in rev(seq_len(length(factors)))) {table <- table[order(table[, i]), ]}  # ordone les donnees par colonnes
  
  table <- data.frame(table)
  colnames(table) <- factors
  table <- na.omit(table)
  
  rownames(table) <- seq_len(nrow(table))

  return(table)
}
