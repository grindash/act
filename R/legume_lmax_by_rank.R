#' Calcule le potentiel de longueur maximale des feuilles (Lmax)
#' Inutile sur les versions recentes de L-egume car cette variable est mise en sortie.
#'
#' @param zip_path chemin de l'archive
#' @param n numero des rangs
#' @param param_path chemin du fichier 'Parametres_plante'. Trouve a partir du chemin de l'archive si non indique
#' @param id_plantes nom de la plante
#'
#' @return data frame
#' @export
#' 
#' @importFrom readxl read_excel cell_cols
legume_lmax_by_rank <- function(zip_path, n, param_path=NULL, id_plantes=NULL) {
  if (is.null(param_path)) param_path <- act::dirname2(zip_path, pattern = "legume")
  param_p_path <- list.files(dirname(param_path), "Parametres_plante_v5cLucas", recursive = T, full.names = T)
  if (length(param_p_path) == 0) stop(paste("Pas de fichier 'Parametres_plante_v5cLucas' dans le dossier", param_path))
  
  if (is.null(id_plantes)) {
    df_usm <- read.csv(unz(zip_path, "liste_usms_mix.csv"), sep = ";")
    id_plantes <- unique(df_usm$ongletP)
  }
  
  for (onglet in id_plantes) {
    if (grep(onglet, id_plantes) == 1) {
      df_param_plante <- data.frame()
      l_max_rank <- NULL
    }
    df <- data.frame(ongletp = onglet, readxl::read_excel(param_p_path, sheet = onglet, range = readxl::cell_cols("A:D")))
    
    larg_max_param <- df[df$name == "profilLeafI_Rlarg", "value"]
    if (larg_max_param[1] < 0) {  # Si le coefficient de la droite est negatif
      nb_rank <- max(n, 50)
      eq1 <- larg_max_param[1] * seq(0, nb_rank) + larg_max_param[2]  # ax + b
      eq2 <- larg_max_param[3] * seq(0, nb_rank) + larg_max_param[4]  # a'x + b'
      switch_eq <- which(eq1 < eq2)[1]  # intersection entre les 2 equations
      
      l_max <- c(eq1[1:switch_eq], eq2[switch_eq:nb_rank])
      l_max_rank <- sapply(n, function(x) l_max[x])
    } else {  # Si le coefficient de la droite est positif
      stop("pas encore prepare lol ^^'")
    }
    df_param_plante <- rbind(df_param_plante, df)
  }
  return(l_max_rank)
}
