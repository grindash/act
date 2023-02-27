#' Trace un boxplot sur les donnees fournies
#'
#' @param data data.frame contenant les donnees
#' @param x parametre de l'axe x
#' @param y parametre de l'axe y
#' @param z ajoute une 3e variable (fonctionne si non fourni)
#' @param xlab nom de l'axe y
#' @param ylab nom de l'axe y
#' @param path chemin de sauvegarde du plot
#' @param ... argument supplementaire de la fonction boxplot()
#'
#' @return plot
#' @export
#'
#' @importFrom pals cols25
act_boxplot <- function(data, x, y, z, xlab=NULL, ylab=NULL, path=NULL, ...) {
  i <- 1
  draw_lg <- TRUE
  
  if (exists("opt", envir = .GlobalEnv)) {
    opt <- get("opt", envir = .GlobalEnv)
  } else {
    opt <- act::default_opt()
  }
  
  if (!is.null(path) & opt$png_output) act::make_output(path, screen_ratio = 1920 / 1080)
  
  if (missing(z)) {
    data$z <- ""
    z <- "z"
    draw_lg <- FALSE
  }
  
  data[, x] <- as.factor(data[, x])
  data[, z] <- as.factor(data[, z])
  list_z <- unique(sort(data[, z]))
  bx_col <- pals::cols25(max(2, length(list_z)))
  
  
  if (draw_lg) par(mar = c(par("mar")[1:3], 4 + 0.3 * max(nchar(as.character(list_z)))))
  
  # Position des barres
  t  <- seq(-0.4, 0.4, length.out = length(list_z) + 1)
  barplot_pos <- c()
  for (n in 2:length(t)) barplot_pos <- c(barplot_pos, mean(t[(n - 1):n]))
  
  # Plot vide
  boxplot(data[, y] ~ data[, x], boxfill = NA, border = NA, 
          xlab = if (is.null(xlab)) x else xlab,
          ylab = if (is.null(ylab)) y else ylab,
          ...)
  
  # Ajout des variables
  for (param_z in list_z) {
    df <- na.omit(data[data[, z] == param_z, c(x, y, z)])
    
    if (nrow(df[!is.na(df[, y]),]) > 0) {
      missing <- setdiff(sort(unique(data[, x])), sort(unique(df[, x])))
      for (m in missing) {
        df_missing <- data.frame(m, -1000, unique(df[, z]))
        colnames(df_missing) <- colnames(df)
        df <- rbind(df, df_missing)
        rm(df_missing)
      }
      
      boxplot(df[, y] ~ df[, x], add = T, xaxt = "n", yaxt = "n", xlim = c(0, 15),
              boxwex = (0.8 / length(list_z)) * 0.9,
              at = 1:(length(unique(df[, x]))) + barplot_pos[i], 
              col = bx_col[i])
    }
    i <- i + 1
  }
  if (draw_lg) legend(x = par("usr")[2] * 1.005, y = par("usr")[4], list_z, pch = 22, pt.cex = 2, pt.bg = bx_col, title = z, xpd = NA)
  if (!is.null(path)) dev.off()
}