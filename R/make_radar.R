#' Create a radar from data frame
#'
#' @param data vector or data frame
#' @param min minimum value
#' @param max maximum value of
#' @param list_angle list of angle. default is 0 to 360°
#' @param fov Numerical. Angle between each value, useless if 'list_angle' is given.
#' @param main If any, main title should be typed.
#' @param col A vector of color codes for plot data
#' @param radar.col A vector of color codes for plot data
#' @param lab.col Color of axis label and numbers: Default "grey"
#' @param na.itp Logical. If true, items with NA values are interpolated from nearest neighbor items and connect them. If false, items with NA are treated as the origin (but not pointed, only connected with lines). Default FALSE.
#' @param ... Miscellaneous arguments to be given for par()
#'
#' @return A radar plot
#' @export
#'
#' @import fmsb
#'
#' @examples
#' make_radar(rnorm(10))
#' make_radar(rnorm(60))
#' make_radar(data.frame(A = rnorm(60), B = rnorm(60)), main = "Exemple")
make_radar <- function(data, min=NULL, max=NULL,
                       list_angle = NULL, fov = NULL,
                       main = NULL, col = NULL, radar.col = "grey", lab.col = 1,
                       na.itp = T, ...) {
  
  if (!is.null(colnames(data))) leg <- colnames(data) else leg <- NULL
  data <- data.frame(data)
    
  if (is.null(list_angle)) { 
    if (is.null(fov)) fov <- 360 / nrow(data)
    list_angle <- seq(0, 360, by = fov)
    list_angle <- list_angle[-length(list_angle)]
  }
  
  # full_na <- as.numeric(which(apply(data, 2, function (x) (is.na(unique(x)) && length(unique(x)) == 1))))
  full_na <- NULL
  for (i in 1:ncol(data)) {
    data[is.infinite(data[, i]), i] <- NA  # remplace les valeurs infini par NA
    
    nb_na <- length(grep("FALSE", is.na(data[, i])))
    nb_tot <- length(list_angle)
    seuil <- 0.3
    if ((nb_na / nb_tot) < seuil) full_na <- c(full_na, i)  # supprime les colonnes qui ont au moins 70% de NA
  }
  
  if (length(full_na) > 0) {
    data <- data[, -full_na]
    leg <- leg[-full_na]
  }
  if (ncol(data) > 0) {
    if (is.null(min)) min <- round(min(data), 3)
    if (is.null(max)) max <- round(max(data), 3)
    
    rownames(data) <- list_angle #defini les azimuts comme le nom des lignes
    data <- t(data) #transpose le colonnes en ligne
    data <- rbind(max = max, min = min, valeur = data)
    data <- as.data.frame(data) #converti en data.frame pour etre utilise par radarchart
    
    par(...)
    blank_radar <- data.frame(matrix(rep(c(20, 0, 10), each = min(ncol(data), 24)), ncol = min(ncol(data), 24)))
    if (is.na(radar.col)) radar.col <- rgb(1, 1, 1, 0)
    fmsb::radarchart(blank_radar, cglcol = radar.col, cglty = 1, vlabels = NA, centerzero = T)
    for (rayon in c(1:4)) {
      # plotrix::draw.circle(0, 0, rayon / 4, lwd = 2, border = "grey")
      if (is.na(lab.col)) text(x = rayon / 4, y = 0, labels = max / (4 / rayon), col = "black", adj = 0)
    }
    par(new = T)
    
    if (is.null(col)) col <- seq_len(ncol(data))
    if (is.na(col[1])) col <- rgb(1, 1, 1, 0)
    if (is.na(lab.col)) lab.col <- rgb(1, 1, 1, 0)
    if (length(full_na) > 0) col <- col[-(1:length(full_na))]
    
    if (file.exists("sink.txt")) sink(NULL)  # couupe les precedentes connexions
    sink("sink.txt")
    fmsb::radarchart(data,
                     title = main,
                     pcol = col,  # couleur des resultats
                     plwd = c(2, 2),  # epaisseur des resultats
                     plty = 1,  # type des resultats
                     axistype = 1, # disposition 1 = echelle de valeur sur le diagramme
                     axislabcol = lab.col, # couleur de l'echelle
                     caxislabels = round(seq(min, max, (max - min) / 4), 3), # Valeur et pas de l'echelle
                     cglcol = NA,  # couleur de la toile
                     cglty = 1,  # epaisseur de la toile
                     vlabels = "",  # etiquettes autour du data
                     centerzero = T,  # centre plein
                     na.itp = na.itp,  # relie les points si il manque des valeurs intermediaires
                     pty = 32)  # 32 = pas de points
    sink(NULL)
    file.remove("sink.txt")
    if (!is.null(leg)) legend("bottomleft", legend = leg, lty = 1, col = col, lwd = 2)
  }
}
