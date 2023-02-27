#' Lecture des fichiers de siclex
#'
#' @param path chemin du fichier
#' @param heure_min supprime les mesures entre minuit et heure_min. defaut 8h00
#' @param heure_max supprime les mesures entre heure_max et minuit. defaut 19h00
#' @param min_par supprime les mesures sous le seuil de PAR
#' @param list_date liste des dates a conserver
#' @param print_unit logical. only print units of siclex if True
#' 
#' @import stringr
#'
#' @return data frame avec les donnees de siclex
#' @export
read_siclex <- function(path, heure_min = 8, heure_max = 19, min_par = 0, list_date = NULL, print_unit = F) {
  column_name <- read.table(path, nrows = 1, skip = 1, sep = "\t")
  column_name[1] <- "date_heure"
    
  if (print_unit) {
    units <- read.table(path, nrows = 1, skip = 4, sep = "\t", col.names = column_name)
    cat("Only print units when 'print_unit' is TRUE\n\n\n")
    siclex <- data.frame(t(units))
    colnames(siclex) <- "units"
    siclex[1, "units"] <- "%d-%m-%Y %H:%M:%S"
  } else {
    siclex <- read.table(path, skip = 9, sep = "\t", col.names = column_name)
    siclex <- siclex[siclex$Ray.PAR.total > min_par, ]
    
    siclex$date <- stringr::str_split_fixed(siclex$date_heure, " ", 2)[,1]
    siclex$heure <- stringr::str_split_fixed(siclex$date_heure, " ", 2)[,2]
    
    if (is.null(list_date) == F) {
      list_date <- apply(stringr::str_split_fixed(unique(list_date), "-", 3)[, c(3,2,1)], 1, paste, collapse = "-")
      siclex <- siclex[siclex$date %in% list_date, ]
    }
    
    # siclex$heure <- format(as.POSIXct(siclex$date_heure, format = "%d-%m-%Y %H:%M:%S"), "%H:%M:%S")
    # siclex$date <- format(as.POSIXct(siclex$date_heure, format = "%d-%m-%Y %H:%M:%S"), "%d-%m-%Y")
    
    # siclex$heure_num <- as.numeric(as.POSIXct(siclex$heure, format = "%H:%M:%S") - as.numeric(as.POSIXct("00:00:00", format = "%H:%M:%S")))
    # siclex <- siclex[siclex$heure_num > heure_min * 3600 & siclex$heure_num < heure_max * 3600, ] # Pas de mesure avant 8h et apres 19h
    
    siclex$Ray.PAR.direct <- siclex$Ray.PAR.total - siclex$Ray.PAR.diffus
    siclex <- act::order_column(siclex, c("date", "heure", "date_heure"))
    
    act::progress_bar(100, 100, "import siclex")
  }
  return(siclex)
}
