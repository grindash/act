#' Import a csv file while removing duplicate row and keeping numerical col as numeric.
#'
#' @param df_path The name of the file which the data are to be read from.
#' @param dec The character used in the file for decimal points.
#'
#' @return data frame
#' @export
#'
#' @import utils
read_csv_clean <- function(df_path, dec=".") {
  if (typeof(df_path) == "character") {
    df <- na.omit(read.csv2(df_path, dec = dec, header = F))
    df_path <- dirname(df_path)
  } else {
    df <- df_path
    df_path <- getwd()
  }

  df <- df[!duplicated(df), ]
  colnames(df) <- apply(df[1, ], 2, as.character)
  df <- df[-1, ]

  write.csv(df, file.path(df_path, "temp_file.csv"), row.names = F)
  df <- read.csv(file.path(df_path, "temp_file.csv"))
  file.remove(file.path(df_path, "temp_file.csv"))

  return(df)
}
