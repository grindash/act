#' Importe les sorties de caribu et recalcule le zeta a partir des mesures
#'
#' @param path chemin des sorties de caribu
#' @param dir_path dossier des mesures de maya, jaz et siclex, uniquement si pas importes au prealable
#' @param spectro data.frame combinant les donnees de maya, jaz et siclex.
#' @param maya chemin vers le fichier ou data.frame des donnees de maya. ignore si spectro non NULL
#' @param siclex chemin vers le fichier ou data.frame des donnees de siclex. ignore si spectro non NULL
#' @param jaz chemin vers le fichier ou data.frame des donnees de jaz. ignore si spectro non NULL
#' @param verbose logical. Print info such as progress bar if True.
#' @param ... args for 'act::caribu_data' function
#'
#' @return data frame
#' @export
#' 
#' @importFrom stringr str_split_fixed
#' @import plyr
import_caribu <- function(path=NULL, dir_path=NULL, spectro=NULL, maya=NULL, siclex=NULL, jaz=NULL, verbose = T, ...) {
  if (is.null(maya) | is.null(siclex) | is.null(jaz)) {
    if (is.null(dir_path)) {
      stop("Donnees introuvable, veuillez renseigner dir_path")
    } else {
      maya <- act::read_maya(list.files(dir_path, "maya_archiduo", full.names = T, recursive = T))
      siclex <- act::read_siclex(list.files(dir_path, "Siclex.*txt", recursive = T, full.names = T), min_par = 100, list_date = unique(maya$date))
      jaz <- act::read_jaz(list.files(dir_path, "jaz.*csv", recursive = T, full.names = T))
    }
  }
  
  if (is.null(path)) {
    df_import <- NULL
    nom_maya <- NULL
  } else {
    caribu_infos <- read_caribu(path)  # caribu_infos contains 2 infos
    if (length(caribu_infos) == 2) {
      df_import <- caribu_infos$df
      nom_maya <- caribu_infos$nom_maya
    } else {
      df_import <- caribu_infos
    }
    df_import <- df_import[!(df_import$sky == "uoc" & df_import$ray == "direct"), ]
  } 
  
  #fusion des donnees
  if (!is.null(spectro)) {
    by <- colnames(df_import)[colnames(df_import) %in% colnames(spectro)]
    if (verbose) cat(paste("Joining by:", paste(by, collapse = ", "), "\n"))
    df_import <- plyr::join(df_import, spectro, by = by)
    if (!is.null(df_import$percent_plant_area)) df_import[df_import$semis == "Blanc", "percent_plant_area"] <- 0
    if (verbose) act::progress_bar(100, 100, "ajout maya")
  } else {
    df_import <- caribu_data(data_caribu = df_import,
                                  data_maya = maya,
                                  data_siclex = siclex,
                                  data_jaz = jaz,
                                  nom_maya = nom_maya,
                                  verbose = verbose)
    df_import <- act::order_column(df_import, colnames(df_import)[colnames(df_import) %in% c("date", "semis", nom_maya, "heure")])
  }
  #calcul du zeta
  if (!is.null(path)) df_import <- caribu_zeta(data = df_import, data_frame = T) else cat("Pas de calcul de CARIBU\n")
  
  return(df_import)
}