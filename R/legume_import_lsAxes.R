#' Importe les fichiers "lsAxes" de L-egume
#'
#' @param lsAxes_list chemin des fichiers
#' @param zip_path chemin de l'archive
#' @param legume_path optionnel. chemin du dossier de legume
#' @param df_usm data.frame des usm
#' @param organs optionnel. organes a etudier
#'
#' @return data frame
#' @export
legume_import_lsAxes <- function(lsAxes_list, zip_path, legume_path = NULL, df_usm = NULL, organs = NULL) {
  if (is.null(df_usm)) {
    usm_filename <- "liste_usms_mix"
    listzip <- unzip(zip_path, list = T)[, 1]
    list_usm_file <- grep(usm_filename, listzip, value = T)
    if (length(list_usm_file) == 0) {
      stop("le fichier 'liste_usms_mix' doit etre dans l'archive au format csv")
    } else {
      if (grepl("csv", list_usm_file) == F & grepl("xls", list_usm_file)) {
        unzip(zipfile = zip_path, files = "liste_usms_mix.xls", exdir = dirname(zip_path))
        df_usm <- readxl::read_xls(file.path(dirname(zip_path), "liste_usms_mix.xls"), sheet = "SimTest")
        write.csv2(data.frame(df_usm), file.path(dirname(zip_path), "liste_usms_mix.csv"), row.names = F)
        act::make_zip(dirname(zip_path), basename(zip_path), "liste_usms_mix.csv")
        file.remove(file.path(dirname(zip_path), "liste_usms_mix.xls"))
        file.remove(file.path(dirname(zip_path), "liste_usms_mix.csv"))
      }
      
      usm_filename <- paste0(usm_filename, ".csv")
    }
    df_usm <- read.csv(unz(zip_path, usm_filename), sep = ";")
  }
  
  lsAxes_full <- data.frame()
  for (lsaxes_file in lsAxes_list) {
    df_lsaxes <- read.csv(unz(zip_path, lsaxes_file), sep = ";")
    
    #recuperation de l'id de simulation
    infos_path <- unlist(strsplit(basename(lsaxes_file), "_"))
    id_usm <- infos_path[grep("l-egume", infos_path) - 1]  # Quelque soit les simulations l'id des usm est situe avant le nom du logiciel
    for (i in unlist(strsplit(lsaxes_file, "_"))) if (nchar(i) == 10) df_lsaxes$date <- i
    
    #Ajout de l'info "densite"
    df_lsaxes$densite <- as.factor(round(100 * 100 / df_usm[df_usm$ID_usm == id_usm, "cote"]^2))  # Transforme la distance entre les plantes en densite
    df_lsaxes$ID_usm <- id_usm
    df_lsaxes$variete <- df_usm[df_usm$ID_usm == id_usm, "ongletP"]
    df_lsaxes$redif <- df_usm[df_usm$ID_usm == id_usm, "bradiatif"]
    df_lsaxes[df_lsaxes$redif == 0, "redif"] <- "direct"
    df_lsaxes[df_lsaxes$redif == 1, "redif"] <- "scattering"
    
    #Ajout de l'info "modele de simulation"
    etr <- df_usm[df_usm$ID_usm == id_usm, "ETR"]
    if (!is.null(etr)) {
      if (etr == 1) {
        df_lsaxes$modele_lum <- factor("RIRI")
        df_lsaxes$modele <- factor("riri")
      }
      if (etr == 2) {
        df_lsaxes$modele_lum <- factor("CANESTRA Face")
        df_lsaxes$modele <- factor("caribu")
      }
      if (etr == 3) {
        df_lsaxes$modele_lum <- factor("CANESTRA Organ")
        df_lsaxes$modele <- factor("caribu")
      }
    }
    
    #Fusion des tableaux
    df_lsaxes <- df_lsaxes[, -1]
    lsAxes_full <- rbind(lsAxes_full, df_lsaxes)
    if (!is.null(organs)) {
      if (!all(organs %in% unique(lsAxes_full$organ))) stop(paste("Organe inexistant, organes possible:", paste(unique(lsAxes_full$organ), collapse = ", ")))
      lsAxes_full <- lsAxes_full[grep(paste(organs, collapse = "|"), lsAxes_full$organ), ]
    }
    
    act::progress_bar(grep(lsaxes_file, lsAxes_list), length(lsAxes_list), "Fusion lsAxes")
  }
  lsAxes_full <- lsAxes_full[!is.na(lsAxes_full$modele_lum), ]
  
  # Calcul de Lmax
  lsAxes_full$Lmax <- NA  # Ajout de la longueur maximum potentielle
  for (var in unique(lsAxes_full$variete)) {
    lsAxes_full[lsAxes_full$variete == var, "Lmax"] <- act::legume_lmax_by_rank(zip_path = zip_path,
                                                                                n = lsAxes_full[lsAxes_full$variete == var, "rank"] + 1,
                                                                                id_plantes = var)
  }
  
  return(lsAxes_full)
}
