#' Ajoute une legende a une figure
#' 
#'
#' @param df data.frame
#' @param z nom de la colonne correspondant aux legendes
#' @param pos position de la legende. Comprend les options de legend() et "out".
#' @param pch symbole des points
#' @param palette couleurs
#' @param max_by_column seuil de separation en plusieurs colonnes
#' @param type "p" ou "l". points par defaut
#' @param title titre de la legende
#' @param second_curve logical. si 2 axes y
#' @param lg_second_curve nom des 2 axes y si second_curve=T
#'
#' @return une legende
#' @export
print_legend <- function(df, z, pos, pch=22, palette=palette(), max_by_column=50, type="p", title="", second_curve=F, lg_second_curve=NULL) {
  unique_z_comb <- act::unique_table(df, z)
  nb_pch <- nrow(table(unique_z_comb))
  
  #Parametres de la legende
  if (length(z) == 1) {
    lg_txt <- unique_z_comb[, z]  # if (is.null(levels(df[, z]))) unique(df[, z]) else levels(df[, z])
    pch_lg <- pch
    nb_color <- ceiling(length(lg_txt) / max_by_column)
    lg_bg_col <- palette
    lty <- 1
    lwd <- rep(NA, length(palette))
  } else {
    z_label_info <- lapply(df[, z], function(i) {if (is.null(levels(i))) unique(i)[order(unique(i))] else levels(i)}) # recupere les ids unique des parametres z 
    z_label_info <- lapply(seq_along(z_label_info), function(i) {z_label_info[[i]] <- c(names(z_label_info[i]), unlist(z_label_info[i]), use.names = F)})   # ajoute le nom des parametres
    z_length_max <- max(lengths(z_label_info)) # taille des colonnes
    
    lg_txt <- lapply(z_label_info, function(i) {c(i, rep(NA, z_length_max - length(i)))})  # rempli les colonnes de NA pour que chaque parametre ai la meme longueur
    lg_txt <- unlist(lg_txt, use.names = F)
    title <- NULL # paste(z, collapse = paste(rep(" ", max(nchar(unlist(lapply(df[, z], unique))))), collapse = ""))
    
    if (type == "l") {
      palette <- c(NA, rep(1, z_length_max - 1), NA, unique(palette))  # couleurs des lignes
      lty <- c(NA, 1:(z_length_max - 1), NA, rep(1, z_length_max - 1)) # forme des lignes
      lty[which(is.na(lg_txt))] <- NA
    } else {
      pch_left_col <- unique(pch)[1:(z_length_max - 1)]
      pch_right_col <- rep(21, z_length_max)
      pch_lg <- c(NA, pch_left_col, NA, pch_right_col)
      pch_lg[which(is.na(lg_txt))] <- NA
      
      lg_bg_col <- c(NA, rep("black", nb_pch), NA, unique(palette))
      
      lwd <- rep(NA, z_length_max * length(z))
    }
    nb_color <- ceiling(length(lg_txt) / max_by_column)
  }
  
  # Ajout de la legende
  if (pos == "out" & act::plot_number() == par("mfrow")[2]) {
    legend(x = par("usr")[2] * 0.99, y = par("usr")[4],
           legend = lg_txt, title = if (length(z) == 1) z else title,
           pch = if (type == "l") NULL else pch_lg,
           col = if (type == "l") palette else "black",
           lty = if (type == "l") lty else NULL,
           lwd = if (type == "l") 2 else lwd,
           pt.bg = if (type == "l") NULL else lg_bg_col,
           bty = "n", pt.cex = 1.2, xpd = NA)
  } else if (pos != "out") {
    if (second_curve) {
      lg_txt <- lg_second_curve
      palette <- c(palette, "black")
      nb_color <- c(nb_color, "black")
      pch_lg <- pch[seq_len(nrow(unique_z_comb) + 1)]
    }
    
    if (length(pos) == 1) {lg.x <- pos; lg.y <- NULL}
    if (length(pos) == 2) {lg.x <- pos[1] * par("usr")[2]; lg.y <- pos[2] * par("usr")[4]}
    legend_args <- list(x = lg.x, y = lg.y,
                        legend = lg_txt,
                        title = title,
                        pch = if (type == "l") NULL else pch_lg,
                        col = if (type == "l") palette else "black",
                        lty = if (type == "l") lty else NULL,
                        lwd = if (type == "l") 2 else lwd,
                        # cex = txt_size,
                        pt.bg = if (type == "l") NULL else lg_bg_col,
                        bty = "n",
                        ncol = if (length(z) == 1) ceiling(length(lg_txt) / 6) else length(z),
                        xpd = T)
    # if (type == "l") legend_args <- c(legend_args, )
    
    # legend_args[which(names(legend_args) %in% lgd_ellipsis)] <- NULL
    # do.call(legend, args = c(legend_args, lgd_ellipsis))
    do.call(legend, args = c(legend_args))
  }
}
