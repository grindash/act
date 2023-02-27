#' Compute zeta from caribu modeling
#' @description Permet de calculer le zeta calcule par caribu en prenant en compte les modification de la serre
#'
#' @param data data frame
#' @param siclex siclex data if not in data
#' @param tau_diffus tau correspond au pourcentage de rayonnement present a l'interieur de la serre en comparaison avec le rayonnement exterieur
#' @param output check details for all possible arguments
#' @param pattern colonne a comparer pour identifier les simulations uniques
#' @param data_frame should the output be a data.frame or a list
#'
#' @details 
#' Calcul zeta = tau * alpha * siclex * caribu\cr
#' tau correspond au pourcentage de rayonnement present a l'interieur de la serre en comparaison avec le rayonnement exterieur\cr
#' alpha correspond au pourcentage d'energie correspondant aux longueur d'ondes choisies par rapport au PAR\cr
#' siclex correspond au PAR mesure en exterieur en direct ou en diffus\cr
#' caribu correspond au rayonnement des sorties de simulation pour rc ou rs\cr
#' Hypothese : alpha est le meme quelque soit le ciel\cr
#' 
#' options for output:\cr
#' rc_direct\cr
#' rs_direct\cr
#' rc_diffus\cr
#' rs_diffus\cr
#' rc_global\cr
#' rs_global\cr
#' global\cr
#' direct\cr
#' diffus\cr
#' brut_global
#' 
#' 
#' @return data frame
#' @export 
caribu_zeta <- function(data, siclex=NULL, tau_diffus=0.54, output = "global", pattern=NULL, data_frame=F) {
  sky <- unique(data$sky)
  nb_direct <- nrow(data[data$ray == "direct",])
  nb_diffus <- nrow(data[data$ray == "diffus",])
  
  #Verifie que chaque condition a une mesure de direct et de diffus
  if (sky == "soc" && nb_direct != nb_diffus) {
    cat('suppression des mesures uniques')
    sum_verif <- 1
    while (sum_verif > 0) {
      # mesure_solo <- setdiff(unique(data$semis), data$semis[duplicated(data$semis)])
      # data <- data[!data$semis %in% mesure_solo, ]
      sum_verif <- 0
      data_clean <- data.frame()
      pattern <- paste(paste0("^", c("date", "semis", "sensor_diameter", "heure_simul"), "$"), collapse = "|")
      conditions <- unique(data[, grep(pattern, colnames(data), value = T)])
      for (i in 1:nrow(conditions)) {
        df <- act::act_call(data, conditions, i)
        verif <- nrow(df) %% 2
        if (verif != 1) {
          data_clean <- rbind(data_clean, df)
        }
        sum_verif <- sum_verif + verif
      }
      data <- data_clean
    }
    nb_direct <- nrow(data[data$ray == "direct",])
    nb_diffus <- nrow(data[data$ray == "diffus",])
  }
  
  if (nrow(data) > 0) {
    #Recuperation des valeurs
    if (nb_direct > 0) {
      rc_direct_brut <- as.numeric(data[data$ray == "direct", "rc_ei"])
      rs_direct_brut <- as.numeric(data[data$ray == "direct", "rs_ei"])
      par_direct <- as.numeric(data[data$ray == "direct", "Ray.PAR.direct"])
    } else {
      rc_direct_brut <- 0
      rs_direct_brut <- 0
      par_direct <- 0
    }
    
    if (nb_diffus > 0) {
      rc_diffus_brut <- as.numeric(data[data$ray == "diffus", "rc_ei"])
      rs_diffus_brut <- as.numeric(data[data$ray == "diffus", "rs_ei"])
      par_diffus <- as.numeric(data[data$ray == "diffus", "Ray.PAR.diffus"])
    } else {
      rc_diffus_brut <- 0
      rs_diffus_brut <- 0
      par_diffus <- 0
    }
    
    # calcul du direct
    par_direct_tau <- 1
    rc_direct_recalc <- par_direct_tau * act::alpha_calc(660, "direct") * par_direct * rc_direct_brut
    rs_direct_recalc <- par_direct_tau * act::alpha_calc(730, "direct") * par_direct * rs_direct_brut
    
    # calcul du diffus
    if (is.null(tau_diffus)) {
      par_diffus_tau <- siclex[siclex$date == "2019-11-04" & siclex$siclex_par_diffus / siclex$siclex_par_total > 0.99, c("jaz_flux_photons_400_700_nm", "siclex_par_total")]  # journee en diffus complet (>0.99)
      par_diffus_tau <- mean(par_diffus_tau$jaz_flux_photons_400_700_nm / par_diffus_tau$siclex_par_total, na.rm = T)
      if (is.null(siclex)) {
        par_diffus_tau <- data[data$Ray.PAR.diffus / data$Ray.PAR.total > 0.99, c("jaz_flux_photons_400_700_nm", "Ray.PAR.total")]
        par_diffus_tau <- mean(par_diffus_tau$jaz_flux_photons_400_700_nm / par_diffus_tau$Ray.PAR.total, na.rm = T)
      }
    } else {
      par_diffus_tau <- tau_diffus
    }
    rc_diffus_recalc <- par_diffus_tau * act::alpha_calc(660, "diffus") * par_diffus * rc_diffus_brut
    rs_diffus_recalc <- par_diffus_tau * act::alpha_calc(730, "diffus") * par_diffus * rs_diffus_brut
    
    # preparation
    output <- switch(output,
                     rc_direct = rc_direct_recalc,
                     rs_direct = rs_direct_recalc,
                     rc_diffus = rc_diffus_recalc,
                     rs_diffus = rs_diffus_recalc,
                     rc_global = rc_direct_recalc + rc_diffus_recalc,
                     rs_global = rs_direct_recalc + rs_diffus_recalc,
                     global = (rc_direct_recalc + rc_diffus_recalc) / (rs_direct_recalc + rs_diffus_recalc),
                     direct = rc_direct_recalc / rs_direct_recalc,
                     diffus = rc_diffus_recalc / rs_diffus_recalc,
                     brut_global = (rc_direct_brut + rc_diffus_brut) / (rs_direct_brut + rs_diffus_brut)
    )
    
    if (is.null(output)) stop("output option incorrect")
    if (data_frame) {
      output <- cbind(data[data$ray == "diffus", ], caribu_zeta = output)
      output$ray <- NULL
    }
    
    # output[output$zeta_maya > 1.3, "caribu_zeta"] <- output[output$zeta_maya > 1.3, "caribu_zeta"] * max(data$zeta_manip) / max(output$caribu_zeta)  # corrige les sous estimations
    
    return(output)
  } else stop("data vide")
}
