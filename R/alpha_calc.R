#' Calcule le parametre alpha pour une longueurs d'onde donnee
#'
#' @param lengthwave numeric
#' @param ray indicate if ray are "direct" or "diffus". permet de recuperer correctement le alpha du fichier de these.
#' @param step numeric interval around lengthwave
#' @param these_path path to Didier Combes csv file
#'
#' @return alpha value
#' @export
#'
#'@importFrom rstudioapi getSourceEditorContext
#'
#' @examples alpha_calc(660, "direct")
alpha_calc <- function(lengthwave, ray="direct", step=5, these_path = "default") {
  # import donnees these Didier
  if (these_path == "default") {
    these_path <- list.files(system.file('extdata', package = 'act'), "diffus_these_combes.csv", full.names = TRUE)
  } 
  if (!file.exists(these_path)) stop("Le chemin jusqu'au fichier de these n'existe pas")
  these <- read.csv(these_path, header = T)
  these$direct_energie <- these$global_energie - these$diffus_energie
  these$direct_flux_photons <- these$global_flux_photons - these$diffus_flux_photons
  
  # calcul du flux de photons pour le PAR
  par <- these[these$nm %in% seq(400, 700, 5), paste0(ray, "_flux_photons")]
  aire_par <- (sum(par) - par[1] / 2  - par[length(par)] / 2) * 5
  
  # calcul du flux de photon pour la longueur d'onde donnee
  lw <- these[these$nm %in% as.character(c(lengthwave - step, lengthwave, lengthwave + step)), paste0(ray, "_flux_photons")]
  alpha_rc <- (sum(lw) - lw[1] / 2  - lw[3] / 2) * 5 / aire_par
  
  return(alpha_rc)
}