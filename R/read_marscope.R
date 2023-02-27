#' Reads a zip containing MARscope data in '.dat' format and creates a data frame from it.
#'
#' @param path Path to the directory name of the zip file or a direct 'dat' file path.
#' @param pattern Pattern of the zip file.
#'
#' @return list of two data frames. One with incident and one with direct measure.
#' @export
read_marscope <- function(path, pattern = "Carac") {
  options(warn = 1)
  
  # Import des fichiers
  if (missing("path")) path <- choose.files()
  if (grepl("zip$", path)) {
    zip_file <- path
  } else {
    zip_file <- list.files(path, paste0(pattern, ".*zip"), full.names = T, recursive = T)
  }
  
  marscope_colnames <- c("date_heure", "cycle", "azimut_solaire", "elevation_solaire", "id",
                         "rayonnement", "Z_sol", "azimut_N", "angle_recepteur", "Z_init", "angle_nord_init_azimut",
                         "incident_associe", "voie_mux", "tps_integration", "energie", "flux_photons", 
                         "efficienceYPF", "uva_bleu", "bleu_elargi", "bleu_strict", "vert", "rouge_clair",
                         "rouge_sombre", "rc_rs", "phi", "teta", "rc", "rs", "zeta")
  
  read_dat <- function(x, zip = NULL) {
    options(warn = -1)
    if (is.null(zip)) {
      df <- read.table(x, sep = ";", skip = 8, fill = T, header = T)
      df_verif <- read.table(x, sep = ";", skip = 8, fill = T, header = F)
    } else {
      df <- read.table(unz(zip, x), sep = ";", skip = 8, fill = T, header = T)
      df_verif <- read.table(unz(zip, x), sep = ";", skip = 8, fill = T, header = F)
    }
    options(warn = 1)
    
    if (ncol(df) < ncol(df_verif)) {
      col_voie <- grep("Voie mux", df_verif[1, ])
      bad_row <- !is.na(df_verif[, ncol(df_verif)])
      for (i in col_voie:ncol(df_verif)) {
        if (i == ncol(df_verif)) {
          df_verif[, i] <- NULL
          colnames(df_verif) <- df_verif[1, ]
          df_verif <- df_verif[-1, ]
        }
        else df_verif[bad_row, i] <- df_verif[bad_row, i + 1]
      }
      df <- df_verif
      df_path <- getwd()
      write.csv(df, "temp_file.csv", row.names = F)
      df <- read.csv("temp_file.csv", header = T)
      file.remove("temp_file.csv")
    }
    return(df)
  }
  
  if (grepl("dat", path)) {  # un seul fichier dat
    cat("Import a partir du .dat\n")
    df_marscope <- read_dat(path)
    df_marscope$Hauteur_sol.mm. <- NULL  # colonne presente sur d'ancienne version du MARscope
    if (ncol(df_marscope) == length(marscope_colnames)) {
      colnames(df_marscope) <- marscope_colnames  
    } else {
      stop("Le nombre de colonnes du fichier ne correspond pas a celui attendu")
    }
  } else {  # ficher zip
    cat("Import a partir du zip\n")
    if (length(zip_file) == 0) stop("Les sorties du MARscope doivent etre dans un zip, indiquer le chemin du repertoire de ce zip")
    list_files <- grep("dat", unzip(zip_file, list = T)[, 1], value = T)
    
    df_marscope <- data.frame()
    for (dat in list_files) {
      df <- read_dat(dat, zip_file)
      df$Hauteur_sol.mm. <- NULL  # colonne presente sur d'ancienne version du MARscope
      colnames(df) <- marscope_colnames
      df_marscope <- rbind(df_marscope, df)
      try(act::progress_bar(grep(dat, list_files), length(list_files), "import fichier dat"), silent = T)
    }
  }
  
  
  # Modification de la colonne date_heure
  date_heure <- as.POSIXct(df_marscope$date_heure, format = "%Y-%m-%d %H:%M:%OS")
  df_marscope$date <- as.Date(format(date_heure, "%Y-%m-%d"))
  df_marscope$heure <- format(date_heure, "%H:%M:%OS")
  df_marscope$date_heure <- NULL
  
  df_marscope <- act::order_column(df_marscope, c("date", "heure"))  # Positionne Date et heure en 1er
  df_marscope <- df_marscope[order(df_marscope$date, df_marscope$id, df_marscope$heure), ]
  
  # Correction des valeurs negatives
  for (col in colnames(df_marscope)[16:29]) df_marscope[df_marscope[, col] < 0 & !is.na(df_marscope[, col]), col] <- NA 
  
  # Recalcule Zeta car certaines cellules sont vides
  df_marscope$zeta <- round(df_marscope$rc / df_marscope$rs, 3) 
  
  # Separation incident et direct
  df_marscope_incident <- df_marscope[df_marscope$rayonnement == "inc", ]
  df_marscope_direct <- df_marscope[df_marscope$rayonnement == "dir", ]
  
  # Attribution d'un numero de Releve aux mesures directes
  df_marscope_direct$releve <- 0
  pos <- which(df_marscope_direct$azimut_N[-1] - df_marscope_direct$azimut_N[-nrow(df_marscope_direct)] < 0) #difference entre nouvel et ancien angle de mesure
  for (r in seq_len(length(pos) + 1)) {
    row_min <- c(0, pos, nrow(df_marscope_direct))[r] + 1
    row_max <- c(0, pos, nrow(df_marscope_direct))[r + 1]
    df_marscope_direct[row_min:row_max, "releve"] <- r
    try(act::progress_bar(r, length(pos) + 1, "Attribution des numeros de mesure"), silent = T)
  }
  
  cat("Return a list with 2 data frames\n")
  mar <- list(df_marscope_direct = df_marscope_direct, df_marscope_incident = df_marscope_incident)
  print(summary(mar))
  return(mar)
}
