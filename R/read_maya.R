#' Importe les donnees de jaz
#' @description import les donnees de spectrometre maya sous la forme d'un data frame. Scinde le nom de l'echantillon en differentes colonnes et ajoute des colonnes "date" et "heure".
#'
#' @param path chemin ou liste de chemin vers des fichiers maya
#' @param nom nomenclarure des noms d'echantillon
#' @param pattern pattern avec lequel scinder les noms d'echantillon
#' @param zip_path indique le nom du zip si le fichier est dans une archive
#' @param list_lw keep specified lengthwave
#' @param lw_mean recupere uniquement les resultats pour des longueurs d'onde precise
#'
#' @import stringr
#'
#' @return data frame
#' @export
read_maya <- function(path, nom=NULL, pattern=NULL, zip_path=NULL, list_lw=NULL, lw_mean=F) {
  if (is.null(nom)) nom <- c("bloc", "distance", "orientation")
  if (is.null(pattern)) pattern <- "-|_|cm"
  
  df_final <- data.frame()
  for (p in path) {
    maya_colnames <- colnames(read.csv(if (is.null(zip_path)) p else unz(zip_path, p), header = T, sep = ";", nrow = 1))
    nb_col_info <- length(grep("X", maya_colnames, invert = T))
    col2remove <- length(grep("X", maya_colnames))
    col2keep <- seq(nb_col_info)
    
    df_full <- read.csv(if (is.null(zip_path)) p else unz(zip_path, p),
                        header = T, sep = ";", na.strings = "NULL",
                        colClasses = c(rep("character", 2), rep("numeric", 11 + col2remove)))
    
    lengthwaves <- as.numeric(gsub("X", "", grep("X", maya_colnames, value = T)))
    df <- df_full[, col2keep]
    for (lw in list_lw) {
      if (lw_mean == T) {
        # Moyenne sur une longueur d'onde
        lw_col <- which(lengthwaves > (lw) & lengthwaves < (lw + 1))
        flux_lw <- data.frame(rowSums(df_full[, lw_col + nb_col_info]))
      } else if (lw_mean == "interpolate") {
        step_interpol <- floor(max(lengthwaves, na.rm = T)) - ceiling(min(lengthwaves, na.rm = T))
        interpol_df <- t(apply(df_full[, grep("X", maya_colnames)], 1, function(x) approx(lengthwaves, x, n = step_interpol)$y))
        colnames(interpol_df) <- approx(lengthwaves, df_full[1, grep("X", maya_colnames)], n = step_interpol)$x
        lw_interpol <- approx(lengthwaves, df_full[1, grep("X", maya_colnames)], n = step_interpol)$x
        
        lw_col <- which(lw_interpol > (lw - 1) & lw_interpol < (lw + 1))
        lw_col <- lw_col[which.min(abs(lw - lw_interpol[lw_col]))]
        flux_lw <- data.frame(interpol_df[, lw_col])
      } else {
        # Mesure la plus proche de la longueur d'onde choisie
        lw_col <- which(lengthwaves > (lw - 1) & lengthwaves < (lw + 1))
        lw_col <- lw_col[which.min(abs(lw - lengthwaves[lw_col]))]
        flux_lw <- data.frame(df_full[, lw_col + nb_col_info])
      }
      # ajout au data.frame
      colnames(flux_lw) <- paste0(lw, "nm")
      df <- cbind(df, flux_lw)
    }
    
    #Modification des colonnes
    colnames(df)[grep("ZETA_655_665_725_735_nm", colnames(df))] <- "zeta_maya"
    colnames(df)[grep("Date", colnames(df))] <- "date"
    
    #separation des infos contenues dans le nom de l'echantillon
    info_ech <- stringr::str_split_fixed(df[, grep("echantil", colnames(df))], pattern, length(nom) + 1)
    for (n in 1:length(nom)) df[, nom[n]] <- info_ech[, n]
    df[, grep("echantil", colnames(df))]  <- NULL

    #separation de la date et de l'heure
    if (unique(nchar(df$date)) == 16) {
      df$heure <- format(as.POSIXct(df$date, format = "%m/%d/%Y %H:%M"), "%H:%M")
      df$date  <- format(as.POSIXct(df$date, format = "%m/%d/%Y %H:%M"), "%Y-%m-%d")
    } else if (unique(nchar(df$date)) == 19) {
      df$heure <- format(as.POSIXct(df$date, format = "%m/%d/%Y %H:%M:%OS"), "%H:%M:%OS")
      df$date  <- format(as.POSIXct(df$date, format = "%m/%d/%Y %H:%M:%OS"), "%Y-%m-%d")
    }

    df_final <- rbind(df_final, df)
    act::progress_bar(grep(p, path), length(path), "read_maya")
  }
  
  df_final <- act::order_column(df_final, c("date", "heure", nom))
  return(df_final)
}
