#' Import a csv file while removing duplicate row and keeping numerical col as numeric.
#'
#' @param df_path Path to data frame or data frame.
#' @param header logical. TRUE if the data frame already has the right header.
#' @param dec The character used in the file for decimal points.
#' @param sep the field separator character. 
#' @param na_omit logical. remove row with NA if True
#'
#' @return data frame
#' @export
#'
#' @import utils
read_csv_clean <- function(df_path, header = T, dec=".", sep = ";", na_omit = T) {
  if (typeof(df_path) == "character") {
    header = F  # force le nom de colonne en 1ere ligne
    df <- read.csv2(df_path, sep = sep, dec = dec, header = header)
    if (na_omit) {
      df_na_filtered <- na.omit(df)
      if (nrow(df_na_filtered) != 0) df <- df_na_filtered
    }
    df_path <- dirname(df_path)
  } else {
    df <- df_path
    if (header) df <- rbind(colnames(df), df)
    df_path <- getwd()
  }

  #remove duplicate row
  df <- df[!duplicated(df), ]
  
  #first line as header
  colnames(df) <- apply(df[1, ], 2, as.character)
  df <- df[-1, ]
  
  if (nrow(df) == 0 & na_omit) stop("Empty data frame. Try with 'na_omit=False'")

  #read again to get the right format
  write.csv(df, file.path(df_path, "temp_file.csv"), row.names = F)
  df <- read.csv(file.path(df_path, "temp_file.csv"))
  file.remove(file.path(df_path, "temp_file.csv"))

  return(df)
}
