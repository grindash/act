#' Importe les fichiers "Compare_Voxel_CaribuOrgane" de L-egume
#'
#' @param zeta_file chemin du fichier 'Compare_Voxel_CaribuOrgane'
#' @param zip_path chemin de l'archive
#'
#' @return data frame
#' @export
legume_import_compare <- function(zeta_file, zip_path) {
  zeta <- data.frame()
  
  for (file in zeta_file) {
    df <- read.csv(unz(zip_path, file), na.strings = "-", sep = ";")
    colnames(df) <- unlist(strsplit(("DOY; ID_usm; organ; age; nump; nsh; rank; ordre; modele_lum; Zeta_Caribu; Zeta_RF; TrPAR; taille_voxel"), "; "))
    zeta <- rbind(zeta, df)
    act::progress_bar(grep(file, zeta_file), length(zeta_file), "Import zeta computation")
  }
  
  # colnames(zeta) <- tolower(colnames(zeta))
  zeta <- zeta[!is.na(zeta$modele_lum), ]
  zeta <- zeta[!is.na(zeta$taille_voxel), ]
  zeta[zeta$modele_lum == 1, "modele_lum"] <- "RIRI"  # "Voxel"
  zeta[zeta$modele_lum == 2, "modele_lum"] <- "CANESTRA Face"  # "Face"
  zeta[zeta$modele_lum == 3, "modele_lum"] <- "CANESTRA Organ"  # "Organ"
  # colnames(zeta)[colnames(zeta) == "modele_lum"] <- "echelle"
  
  return(zeta)
}