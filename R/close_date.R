#' Join two data frame together by their closest date and hour
#' @description Check the time in each row in x and take the row with the closest time in y and merge them.
#'
#' @param x data frame. must contain an "date" and a "heure" column (case sensitive)
#' @param y data frame. must contain an "date" and a "heure" column (case sensitive)
#' @param format_y character string. The default for the format methods is "\%Y-\%m-\%d \%H:\%M:\%S"
#' @param label  text to write in the progress bar. default is "close_date"
#' @param verbose logical. Print the progress bar if True.
#' 
#' @return data frame
#' @export
close_date <- function(x, y, format_y=NULL, label="close_date", verbose = T) {
  # Verification du format des donnees
  if (length(grep("date", colnames(x))) == 0 | length(grep("heure", colnames(x))) == 0) {
    warning("x must have 'date' and 'heure' column")
  }
  if (length(grep("date", colnames(y))) == 0 | length(grep("heure", colnames(y))) == 0) {
    warning("y must have 'date' and 'heure' column")
  }
  
  if (is.null(format_y)) format_y <- act::get_date_format(paste(y$date, y$heure))
  
  # if (is.null(y$date_heure)) {
  y$date_heure <- as.numeric(strptime(paste(y$date, y$heure), format_y))
  # } else {
  # y$date_heure <- as.numeric(strptime(y$date_heure, format_y))
  # }
  if (length(unique(y$date_heure)) == 1 && is.na(unique(y$date_heure))) stop("format de date incorrect")
  
  # Standardisation du format de date
  if (grepl("%d-%m-%Y", format_y)) {
    y$date <- apply(stringr::str_split_fixed(y$date, "-", 3)[, c(3,2,1)], 1, paste, collapse = "-")
  }
  
  x <- x[!is.na(x$date), ]
  x <- x[!is.na(x$heure), ]
  x$date_heure <- as.numeric(as.POSIXct(paste(x$date, x$heure)))
  y <- y[!is.na(y$date), ]
  
  # Calcul de la date la plus proche
  for (d in unique(x$date)) {
    if (grep(d, unique(x$date)) == 1) df_tps <- data.frame()
    
    df_one <- x[x$date == d, ]
    df_two <- y[y$date == d, ]
    
    if (nrow(df_two) == 0) {
      warning(paste0("Pas de date correspondante pour ", d, ". Utilisation de la date la plus proche."))
      d2 <- unique(y$date)[which.min(abs(as.numeric(as.POSIXct(d)) - as.numeric(as.POSIXct(unique(y$date)))))]
      df_two <- y[y$date == d2, ]
      df_two$date_heure <- as.numeric(as.POSIXct(paste(d, df_two$heure)))
    }
      
    for (i in 1:nrow(df_one)) {
      tps_diff <- abs(df_one[i, "date_heure"] - df_two$date_heure)
      tps_min <- which(tps_diff ==  min(tps_diff, na.rm = "NA"))[1]
      tps_proche <- df_two[tps_min, -grep("date|heure", colnames(df_two))]
      
      df <- cbind(df_one[i, ], tps_proche)
      df_tps <- rbind(df_tps, df)
    }
    if (verbose) act::progress_bar(grep(d, unique(x$date)), length(unique(x$date)), paste(label, d))
  }
  
  return(df_tps)
}
