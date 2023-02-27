#' Recupere les informations situees dans un nom de fichier
#'
#' @param data string of data.frame to look for the pattern. Add the pattern in a new column if it is a data.frame
#' @param pattern str. pattern to find in the string
#' @param split Optional str or logical. Remove the "split" from the pattern found
#' @param col_search Optional is data is not a data.Frame. Column to get the pattern.
#' @param new_colname Optional. If data is a data.frame, change the name of the new column.
#'
#' @return data.frame or list depending on 'data'
#' @export
#'
#' @examples get_name_info("This_is_a_test_give_id950", "id", split= TRUE)
get_name_info <- function(data, pattern, split=NULL, col_search=NULL, new_colname=NULL) {
  if (is.null(ncol(data))) {
    data_info <- data
  } else if (is.null(col_search)) {
    stop("data is a data.frame, col_search can't be NULL")
  } else data_info <- data[, col_search]
  
  for (pat in pattern) {
    list_info <- tools::file_path_sans_ext(basename(data_info))
    info_name <- stringr::str_split_fixed(list_info, "_", length(unlist(strsplit(list_info[1], "_"))))
    info <- info_name[, grep(pat, info_name[1, ])]
    
    if (!is.null(split) && split != F) {
      if (split == T) split <- pattern
      info_value <- stringr::str_split_fixed(info, split, 2)
      info_value <- paste0(info_value[, 1], info_value[, 2])
    } else {
      info_value <- info
    }
    if (length(info_value) == 0) info_value <- NA
    
    if (!is.null(ncol(data))) {
      if (is.null(new_colname)) {
        data[, pat] <- info_value
        data <- act::order_column(data, pat)
      } else {
        data[, new_colname[grep(pat, pattern)]] <- info_value
        data <- act::order_column(data, new_colname[grep(pat, pattern)])
      }
    } else {
      data <- info_value
    }
  }
  
  return(data)
}
