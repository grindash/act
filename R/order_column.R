#' Put the selected columns at the beginning of the dataset.
#'
#' @param data data set
#' @param col_names columns to put first
#' @param pos 'first' puts the columns at the beginning of the data.frame, 'last' puts the columns at the end of the data.frame
#'
#' @return data frame
#' @export
#'
#' @examples 
#' data("iris")
#' order_column(iris, "Species")
order_column <- function(data, col_names, pos="first") {
  pos <- tolower(pos)
  col_names <- unique(col_names)
  check_columns <- col_names[!col_names %in% colnames(data)]
  if (length(check_columns) > 0) stop(paste("Colonnes", paste(check_columns, collapse = ", "), "introuvables"))
  
  pattern <- paste0("^", col_names, "$", collapse = "|")
  other_columns <- grep(pattern, colnames(data), value = T, invert = T)
  data <- switch(pos, first = data[, c(col_names, other_columns)], last = data[, c(other_columns, col_names)])
  

# From https://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe/18339562#18339562
# Allows you to move columns to either the first or last position, or to be before or after another column.
# OrderColumn <- function(data, col_names, where="last", ba=NULL) {
#   temp <- setdiff(names(data), col_names)
#   x <- switch(
#     where,
#     first = data[c(col_names, temp)],
#     last = data[c(temp, col_names)],
#     before = {
#       if (is.null(ba)) stop("must specify ba column")
#       if (length(ba) > 1) stop("ba must be a single character string")
#       data[append(temp, values = col_names, after = (match(ba, temp)-1))]
#     },
#     after = {
#       if (is.null(ba)) stop("must specify ba column")
#       if (length(ba) > 1) stop("ba must be a single character string")
#       data[append(temp, values = col_names, after = (match(ba, temp)))]
#     })
#   return(x)
# }

  return(data)
}
