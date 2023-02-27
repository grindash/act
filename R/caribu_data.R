#' Join Caribu data with field data
#' @description Fusionne les donnees de simulation avec les mesures de maya et de siclex.
#'
#' @param data_caribu donnees de simulation. chemin vers un fichier ou data frame
#' @param data_maya donnees de mesure directionnelles. data frame
#' @param data_jaz onnees de mesure hemispheriques data frame
#' @param data_siclex donnees exterieur. data frame
#' @param nom_maya maya name to pass to the read_maya function
#' @param verbose print text or not
#' @param fuse_by colonnes a prendre en commun pour la fusion des donnees
#'
#' @import plyr
#'
#' @return data frame
#' @export
#'
caribu_data <- function(data_caribu=NULL, data_maya=NULL, data_jaz=NULL, data_siclex=NULL, fuse_by=NULL, nom_maya=NULL, verbose = T) {
  heure_format <- function(data, heure_col="heure") {
    if (is.null(data$heure)) {
      if (grepl("%H", act::get_date_format(data$date))) {
        data$date_heure <- data$date
        data$heure <- format(as.POSIXct(data$date), format = "%H:%M:%S")
        data$date <- format(as.POSIXct(data$date), format = "%Y-%m-%d")
      } else stop("Missing column 'heure'")
    }
    mauvais_format <- which(nchar(data[, heure_col]) == 5)
    if (act::get_date_format(data$heure) == "%H:%M") data$heure <- format(as.POSIXct(data$heure, format = "%H:%M"), format = "%H:%M:%S")
    return(data)
  }
  
  # Verifie le format d'entree
  if (is.null(data_caribu)) {
    no_caribu <- TRUE
    data_caribu <- data.frame()
  } else {
    no_caribu <- FALSE
    if (is.character(data_caribu)) {
      cat("read_caribu\n")
      data_caribu <- read.csv(data_caribu, sep = ";", header = T)
    }
    data_caribu <- act::read_csv_clean(data_caribu)
    data_caribu <- heure_format(data_caribu)
    # colnames(data_caribu)[grep("heure", colnames(data_caribu))] <- "heure_simul"  # Attention a cette ligne !
    data_caribu$distance <- as.numeric(data_caribu$distance)
  }
  # Ajout des donnees de maya
  if (is.character(data_maya)) {
    cat("read_maya\n")
    data_maya <- act::read_maya(data_maya, nom = nom_maya)
  }
  data_maya <- heure_format(data_maya)
  data_maya$distance <- as.numeric(data_maya$distance)
  
  # Colonnes communes entre les 2 data.frame
  if (is.null(fuse_by)) fuse_by <- colnames(data_caribu)[colnames(data_caribu) %in% colnames(data_maya)]
  for (n in fuse_by) {
    if (T %in% (unique(data_caribu[, n]) %in% unique(data_maya[, n])) == F) grep(n, fuse_by, invert = T, value = T)
  }
  
  if (verbose) cat(paste("Joining by:", paste(fuse_by, collapse = ", "), "\n"))
  data_join <- plyr::join(data_maya, data_caribu, by = fuse_by)
  if (!no_caribu) {
    data_join <- data_join[!is.na(data_join$ray), ]
    if (nrow(data_join) == 0) data_join <- act::close_date(x = data_caribu, y = data_maya, label = "ajout maya par date proche")
    if (nrow(data_join) == 0) stop("Pas de correspondance entre maya et caribu")
  }
  if (! is.null(data_join$percent_plant_area)) data_join[data_join$semis == "Blanc", "percent_plant_area"] <- 0
  if (verbose) act::progress_bar(100, 100, "ajout maya")
  
  # Ajout des donnees de jaz
  if (is.character(data_jaz)) {
    cat("read_jaz\n")
    if (length(data_jaz) > 1) data_jaz <- read_jaz(data_jaz) else data_jaz <- read_jaz(data_jaz, "jaz.*csv")
  }
  data_jaz <- heure_format(data_jaz)
  if (! is.null(data_jaz)) data_join <- act::close_date(x = data_join, y = data_jaz, label = "ajout jaz", verbose = verbose)
  
  # Ajout des donnees de siclex siclex
  if (is.character(data_siclex)) {
    cat("read_siclex\n")
    data_siclex <- act::read_siclex(data_siclex, min_par = 100, list_date = unique(data_maya$date))
  }
  data_siclex <- heure_format(data_siclex)
  if (! is.null(data_siclex)) data_join <- close_date(x = data_join, y = data_siclex, label = "ajout siclex", verbose = verbose)
  
  return(data_join)
}
