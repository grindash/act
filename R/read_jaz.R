#' Importe les donnees de jaz
#' @description import les donnees de spectrometre jaz sous la forme d'un data frame et ajoute des colonnes "date" et "heure"
#'
#' @param path character string of a path name. can be to a file or a directory
#' @param pattern an optional regular expression
#' @param zip_path path to zip file if the file is in an archive
#' 
#' @return data frame
#' @export
read_jaz <- function(path, pattern=NULL, zip_path=NULL) {
  if (!is.null(pattern)) path <- list.files(path, pattern, recursive = T, full.names = T)
  
  for (jaz in path) {
    if (grep(jaz, path) == 1) df_jaz <- data.frame()
    
    if (is.null(zip_path)) {
      col2remove <- length(grep("X", colnames(read.csv(jaz, header = T, sep = ";", nrow = 1))))
      df <- read.csv(jaz, header = T, sep = ";",
                     colClasses = c(rep("NULL", 1), rep("character", 11), rep("NULL", col2remove)),
                     na.strings = "NULL")
    } else {
      col2remove <- length(grep("X", colnames(read.csv(unz(zip_path, jaz), header = T, sep = ";", nrow = 1))))
      df <- read.csv(unz(zip_path, jaz), header = T, sep = ";",
                     colClasses = c(rep("NULL", 1), rep("character", 11), rep("NULL", col2remove)),
                     na.strings = "NULL")
    }
    
    df <- act::read_csv_clean(df, header = T)
    df_jaz <- rbind(df_jaz, df)
    act::progress_bar(grep(jaz, path), length(path), "import jaz")
  }
  
  colnames(df_jaz) <- tolower(colnames(df_jaz))
  colnames(df_jaz)[2:11] <- paste0("jaz_", colnames(df_jaz)[2:11])
  
  if (unique(nchar(df_jaz$date)) == 16) {
    df_jaz$date_heure <- as.POSIXct(df_jaz$date, format = "%d/%m/%Y %H:%M")
    df_jaz$date  <- format(as.POSIXct(df_jaz$date_heure), "%Y-%m-%d")
    df_jaz$heure <- format(as.POSIXct(df_jaz$date_heure), "%H:%M")
  } else if (unique(nchar(df_jaz$date)) == 19) {
    df_jaz$date_heure <- as.POSIXct(df_jaz$date, format = "%m/%d/%Y %H:%M:%OS")
    df_jaz$date  <- format(as.POSIXct(df_jaz$date_heure), "%Y-%m-%d")
    df_jaz$heure <- format(as.POSIXct(df_jaz$date_heure), "%H:%M:%OS")
  }
  
  df_jaz <- act::order_column(df_jaz, c("date_heure", "date", "heure"))
  return(df_jaz)
}