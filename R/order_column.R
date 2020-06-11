#' Put the selected columns at the beginning of the dataset.
#'
#' @param data data set
#' @param col_names columns to put first
#'
#' @return
#' @export
#'
#' @examples data("iris")
#' @examples order_column(iris, "Species")
order_column <- function(data, col_names) {
  data <- data[, c(col_names, colnames(data)[grep(paste(col_names, collapse = "|"), colnames(data), invert = T)])]

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
#
# Examples:
# moveMe(df, c("b", "c"))
# moveMe(df, c("b", "c"), "first")
# moveMe(df, c("b", "c"), "before", "e")
# moveMe(df, c("b", "c"), "after", "e")

  return(data)
}
