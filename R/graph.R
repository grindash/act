#' Plot following Annals of Botany rules
#'
#' @param x character. colname of the set of data to draw
#' @param y character. colname of the set of data to draw
#' @param z character. colname of the set of data to draw. Default is NA. Can have a length of 2 colnames.
#' @param data data frame. First three columns must be the factors parameters.
#' @param loopsize number of factor
#' @param mfrow plot number by graphical window. c(ncol, nrow)
#' @param new_par logical. If False, changes of the graphical window are ignored. Can have decimal to adjust plot letters if needed.
#' @param type what type of plot should be drawn. Default is points
#' @param lty The line type.
#' @param palette 1 = black and white palette, 2 = color palette. Other palette are accepted if fully detailed : palette(new.palette)
#' @param main individual plot title
#' @param main_title global plot title if multiple plots are printed
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param txt_size size of the text in the plot
#' @param outer logical. If TRUE only one xlab and ylab are writen for all the plot. If FALSE, each plot get the xlab and ylab.
#' @param xlim Plot limit. Either c(min, max), NULL, TRUE or "ylim". If TRUE all the plot will have the same value, if NULL each plot have their own value; if "ylim", xlim will match ylim.
#' @param ylim Plot limit. Either c(min, max), NULL, TRUE, "xlim" or "row. If TRUE all the plot will have the same value, if NULL each plot have their own value; if "xlim", ylim will match xlim; if "row" each plot of the same row of the graphic window will have the same ylim.
#' @param pt_labels chr. put label on each point. chr must be a colname
#' @param lg.title a character or expression vector of length 1 to appear in the legend.
#' @param lg.pos coordinate to position the legend
#' @param lg.nb numeric. plot number where the legend should be displayed. Default is on all plots.
#' @param eq Print an equation on the plot from equation coefficients. Format is "c(coef_1 = a, coef_2=b, ...)"
#' @param hist logical. If TRUE display the distribution of the data.
#' @param mean logical. If TRUE the mean of the y is used instead of raw value.
#' @param interval logical. If TRUE display the confidence interval of the y mean
#' @param errorbar logical. If TRUE display the error bar of the y mean
#' @param rrmse Print rRMSE value on the plot.
#' @param path Path where the plot are saved as png. If empty open a new window instead.
#' @param reg Draw the linear regression of the plot. If "nls" or "lm" return a second order regression. If numeric, return a regression from the specified order, using the lm function.
#' @param reg_col regression color
#' @param reg.itc numeric. Force the intercept of the regression
#' @param reg_ann logical. if TRUE print the equation of the regression on the plot
#' @param reg_pos regression position on the plot
#' @param reg_pos2 regression position on the plot
#' @param reg_output 1 draws a regression onto the current plot. 2 writes the regression parameters in a data frame. 4 do both.
#' @param mult_dev Separate the plot into multiple graphic panel for each unique factor 1.
#' @param x2 second set of data to draw on the same plot
#' @param y2 second set of data to draw on the same plot
#' @param xlim2 plot parameter of the second data set
#' @param ylim2 plot parameter of the second data set
#' @param xlab2 a title for the x axis of the 2nd plot
#' @param ylab2 a title for the y axis of the 2nd plot
#' @param ... additional par() arguments 
#' @param opt options for the graph
#'
#' @return a list of plot
#' @export
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import hydroGOF
#' @import methods
graph <- function(data, x, y, z = NA, loopsize = 0,
                  mfrow = NULL, new_par = T, outer = TRUE, txt_size = 1,
                  type = "p", lty = NULL, palette = 2,
                  main = NULL, main_title = NULL,
                  xlab = NULL, ylab = NULL,
                  xlim = TRUE, ylim = TRUE, pt_labels = NULL,
                  lg.title = "", lg.pos = "bottomright", lg.nb = c(1:1000),
                  mean = FALSE, interval = FALSE, errorbar = FALSE,
                  reg = FALSE, reg_output = 1,
                  reg.itc = NULL, reg_col = "black",
                  reg_pos = "left", reg_pos2 = "right", reg_ann = T, 
                  hist = FALSE, eq = NULL, rrmse = F,
                  x2 = NULL, y2 = NULL,
                  xlim2 = NULL, ylim2 = NULL,
                  xlab2 = NULL, ylab2 = NULL,
                  opt = NULL, path = FALSE, mult_dev = FALSE, ...) {
  options(warn = 1)
  
  if (x %in% colnames(data) == FALSE) stop(paste0("Pas de colonne '", x, "'"))
  if (y %in% colnames(data) == FALSE) stop(paste0("Pas de colonne '", y, "'"))
  if (length(z) == 1 && !is.na(z) && z %in% colnames(data) == FALSE) stop(paste0("Pas de colonne", z, "'"))
  
  if (is.null(opt)) {
    if (exists("opt", envir = .GlobalEnv)) {
      opt <- get("opt", envir = .GlobalEnv)
    } else {
      opt <- act::default_opt()
    }
  }

  second_curve <- length(c(x2, y2)) > 0 | length(x) > 1 | length(y) > 1 
  
  # Modification des donnes ####
  if (length(z) == 1 && is.na(z)) {
    data$na <- 1
    z <- "na"
    if (second_curve == F) lg.nb <- 0
  } else if (length(z) > 2) stop("'z' doit avoir une taille de 2 maximum")
  
  # data <- act::order_column(data, c(x, y, z, x2, y2), pos = "last")
  if (loopsize == 0) {
    data$loop <- 1
    data <- act::order_column(data, c("loop", x))
  }
  
  if (second_curve) {
    x_line <- length(x2) == 1 && is.numeric(x2)
    y_line <- length(y2) == 1 && is.numeric(y2)
    
    if (length(x2) == 0) x2 <- x
    if (length(y2) == 0) y2 <- y
    
    if (length(type) == 1) type <- rep(type, 2)
    
    data_second_curve <- data
    if (mean) data_second_curve <- act::get_data_mean(data_second_curve, x2, y2, NA, loopsize)
  }
  
  if (mean) data <- act::get_data_mean(data, x, y, z, loopsize)
  
  conditions <- act::unique_table(data, colnames(data)[1:loopsize])
  x_lim <- xlim
  y_lim <- ylim

  for (cond in seq_len(nrow(conditions))) {
    if (new_par) act::progress_bar(cond, nrow(conditions), paste(colnames(conditions), collapse = " "))
    
    # Data ####
    if (mean) {
      df <- act::act_call(data, conditions, cond)
      if (second_curve) df2 <- act::act_call(data_second_curve, conditions, cond)
    } else {
      df <- na.omit(act::act_call(data, conditions, cond)[, c(x, y, z, pt_labels)])
      if (second_curve) df2 <- na.omit(act::act_call(data, conditions, cond)[, c(x2, y2, z, pt_labels)])
    }
    
    # Axis limits
    if (length(levels(data[,x])) > 0) xlim <- NULL
    if (length(x_lim) == 0) xlim <- c(0, act::min_max(df[, x])[2])
    if (length(x_lim) == 1) {
      if (x_lim == "ylim") {xlim <- ylim ; y_lim <- "xlim"}
      if (x_lim == T) xlim <- act::min_max(data[, x])
    }
    
    if (length(y_lim) == 1) {
      if (y_lim == T) ylim <- c(0, act::min_max(data[, y])[2])
      if (y_lim == "row" & (act::plot_number() == 1 | act::plot_number("x") == par("mfrow")[2])) ylim <- c(0, act::min_max(act::act_call(data, conditions[,1:(loopsize - 1)], cond)[, y])[2])
      if (y_lim == "xlim") {
        ylim <- c(0, act::min_max(df[, y])[2])
      }
    } else {
      # if (loopsize > 0) ylim <- c(0, act::min_max(act::act_call(data, conditions[, seq(1, loopsize - 1)], cond)[, y])[2])
    }
    
    if (rrmse) {
      xlim <- act::min_max(c(xlim, ylim))
      ylim <- act::min_max(c(xlim, ylim))
    }
    
    # Fenetre graphique
    if (mult_dev) {
      cond_one <- grep(paste0("^", conditions[cond, 1], "$"), unique(conditions[, 1]), value = T)
      if (loopsize == 0) mfrow <- c(1, 1) else mfrow <- act::act_mfrow(data[data[, 1] == cond_one, ], 1:loopsize)
    } else if (length(mfrow) == 0) {
      if (loopsize == 0) mfrow <- c(1, 1) else mfrow <- act::act_mfrow(data, 1:loopsize)
    }

    if (new_par) {
      if (cond == 1) {
        if (length(path) > 0) {
          if (path !=  F & opt$png_output) act::make_output(path, mfrow = mfrow, size = 1600)
        }
        
        par(mfrow = mfrow, mar = c(3, 4, 3, 2), # distance entre les plots
            # oma = rep(c(2, 2) * c(1, (1 + length(y2) / 2)), 2), # distance autour de la zone des plots
            oma = c(rep(2, 3), (2 + length(y2) * 2)), # distance autour de la zone des plots
            ...)
        
        # Check if the plot can be printed
        plot_x <- dev.size("px")[2] / par("mfrow")[1]
        plot_y <- dev.size("px")[1] / par("mfrow")[2]
        if (plot_x < 120 | plot_y < 85) {
          par(mfrow = c(1, 1))
          warning(paste0("Plot margin too large, mfrow set as c(1,1) instead of c(", mfrow[1], ", ", mfrow[2], ")"))
        }
      } else if (mult_dev & conditions[cond, 1] != conditions[cond - 1, 1]) {
        if (length(path) > 0) {
          if (path !=  F & opt$png_output) act::make_output(path, mfrow = mfrow, size = 1600)
        }
        
        par(mfrow = mfrow, mar = c(3, 4, 3, 2),  # distance entre les plots
            oma = rep(c(2, 2) * c(1, (1 + length(y2) / 2)), 2),  # distance autour de la zone des plots
            ...)
        
        if (lg.pos == "out") par(oma = c(par("oma")[1:3], par("oma")[4] + 0.3 * max(nchar(as.character(data[, z])))))
      }
    }

    if (nrow(df) > 0) {
    # Points properties ####
    unique_z_comb <- act::unique_table(df, z)
    nb_pch <- nrow(table(unique_z_comb))
    if (length(z) == 1) {
      nb_color <- nb_pch
      if (length(palette) == 1 && palette == 1) bg_col <- act::shades_of_grey(nb_pch)
      if (length(palette) == 1 && palette == 2) bg_col <- pals::cols25(max(2, nb_pch))
      if (typeof(palette) == "character") {if (length(palette) == 1) bg_col <- rep(palette, 2) else bg_col <- palette}
      pch <- rep(c(21:24), ceiling(nb_pch / 4))
      if (is.null(lty)) {if (type[1] == "p") lty <- rep(0, 100) else lty <- rep(1, 100)}
    }

    if (length(z) == 2) {
      pch <- NULL  # forme des points
      bg_col <- NULL  # couleur 
      for (n in seq_len(nb_pch)) {
        # nombre de couleurs pour la 1ere valeur de z
        nb_color <- rowSums(table(unique_z_comb))[n]
        
        # nombre de forme pour la 2nde valeur de z
        pch <- c(pch, rep((21:24)[n], nb_color))
        if (type[1] == "l") lty <- c(lty, rep((1:6)[n], nb_color))
        if (length(palette) == 1 && palette == 1) ls_col <- act::shades_of_grey(nb_color)
        if (length(palette) == 1 && palette == 2) ls_col <- pals::cols25(nb_color)
        # if (palette == 2) ls_col <- colorRampPalette(c("white", pals::cols25(max(2, nb_color + 1))[n]))(nb_color + 1)[-1]
        if (typeof(palette) == "character") {if (length(palette) == 1) ls_col <- rep(palette, nrow(df)) else ls_col <- palette[1:nb_color]}
        bg_col <- c(bg_col, ls_col)
        if (length(ls_col) == max(rowSums(table(unique_z_comb)))) lg_col <- ls_col
      }
    }
    if (type[1] == "p") lty <- rep(lty, ceiling(nb_color / 4))
    
    # Graph labels ####
    if (loopsize == 0) title <- ""
    if (loopsize == 1) title <- as.character(conditions[cond, ])
    if (loopsize > 1) title <- paste(apply(conditions[cond, ], 2, as.character), collapse = " ")
    if (!is.null(main)) title <- main
    
    if (length(xlab) == 0) xlab <- x
    if (length(ylab) == 0) ylab <- y
    
    # Second Plot ####
    if (second_curve) {
      if (x_line | y_line) leg_second_curve <- FALSE
      else if (nrow(df2[, c(x2, y2)]) > 0) {
        if (length(xlim2) == 1 && xlim2 == T) xlim2 <- xlim
        if (length(ylim2) == 1 && ylim2 == T) ylim2 <- ylim

        df2 <- df2[order(df2[, x2]), ]
        plot(df2[, c(x2, y2)], 
             xlim = xlim2, ylim = ylim2,
             xlab = xlab2, ylab = ylab2,
             type = type[2],
             pch = pch[nrow(unique_z_comb)] + 1,
             col = bg_col[2], #"black",
             lwd = if (type[2] == "l") 2 else 1,
             bg = bg_col[nrow(unique_z_comb) + 1],
             ann = F, xaxt = "n", yaxt = "n", bty = "n")
        
        if (x != x2) {axis(side = 3, las = 1, labels = T)}
        if (y != y2) {axis(side = 4, las = 1, labels = T)}
        
        par(new = T)
        leg_second_curve <- TRUE
      }
      abline(if (x_line) h = x2, if (y_line) v = y2)
    } else leg_second_curve <- FALSE

    # Distribution Histogram ####
    if (hist == T) {
      res <- hist(df[, x], breaks = seq(xlim[1], xlim[2], (xlim[2] - xlim[1]) / 10), yaxt = "n", xaxt = "n", ann = F)
      text(x = res$mids, y = (res$counts) + (max(res$counts) / 35), labels = paste0(round(100 * res$counts / sum(res$counts)), "%"), xpd = NA)
      par(new = T)
    }

    # Main Plot ####
    if (length(unique(data[, x])) < 10) x_is_date <- !all(is.na(as.Date(as.character(df[, x]), format = "%Y-%m-%d"))) else x_is_date <- F
    plot_args <- list(x = df[, c(x, y)],
                      xlim = xlim, ylim = ylim,
                      xlab = "", ylab = "", 
                      las = 1, mgp = c(3, 0.8, 0),  # "mgp" control the axis position: c(axis title, axis label, axis line)
                      type = "n", xaxt = if (x_is_date) "n",
                      main = title, cex.axis = txt_size)
    
    do.call(plot, args = c(plot_args))
    # do.call(plot.default, args = c(plot_args))
    # axis(side = 1, las = 3, labels = unique(data[, x]), at = unique(data[, x]))
    if (x_is_date && length(unique(data[, x])) < 10) axis.Date(1, at = unique(data[, x]))
    # text(seq_along(unique(df[, x])), labels = sort(unique(df[, x])), srt = 45, adj = 1, xpd = TRUE)
    
    for (l in seq_len(nrow(unique_z_comb))) {
      lines(act::act_call(df, unique_z_comb, l)[, c(x, y)], 
            type = type[1],
            pch = pch[l],
            lty = lty[l],
            bg = if (second_curve) bg_col[1] else bg_col[l],
            col = if (type[1] == "l") bg_col[l] else "black",
            lwd = if (type[1] == "l") 2 else 1)
    }
    nbplot <- act::plot_number()
    
    # Labels ####
    if (!is.null(pt_labels)) text(df[, x], df[, y], labels = df[, pt_labels], cex = 0.7, pos = 3)
    fig_per_page <- par("mfrow")[1] * par("mfrow")[2]
    if (fig_per_page > 1) {
      lab_letter <- paste0(rep(LETTERS, ceiling(nbplot / 26))[nbplot], if (fig_per_page >= 26) ceiling(nbplot / 26.01))
      text(x = par("usr")[1],
           y = par("usr")[4] + (par("usr")[4] - par("usr")[3]) * 0.075,
           labels = lab_letter,
           cex = 1.5, xpd = NA, adj = c(0, 1))
    }

    if ((nbplot == 1 & outer == T) | outer == F) {
      mtext(xlab, side = 1, line = if (outer == F) 2 + length(unlist(strsplit(xlab, "\n"))) else 0, cex = txt_size, outer = outer)
      mtext(ylab, side = 2, line = if (outer == F) 0 + length(unlist(strsplit(xlab, "\n"))) else -1, cex = txt_size, outer = outer) #, las = 1)
      if (length(y2) > 0 && is.character(y2)) {mtext(if (length(ylab2) == 0) y2 else ylab2, side = 4, line = 1, cex = txt_size, outer = outer)}
    }
    
    if (nbplot == 1 & !is.null(main_title)) mtext(main_title, cex = 1.1, outer = T) #, las = 1)

    # Legend ####
    if (nbplot %in% lg.nb) act::print_legend(df, z, pos = lg.pos, pch = pch,
                                             title = lg.title,
                                             palette = bg_col,
                                             max_by_column = if (lg.pos != "out") 6,
                                             type = type[1],
                                             second_curve = leg_second_curve,
                                             lg_second_curve = c(y, y2))
    
    # Affichage des erreurs ####
    if (mean && (interval | errorbar)) draw_error(df, x, y, z, interval, errorbar)

    # Regression ####
    if (rrmse) {abline(0, 1); mtext(paste0("rRMSE = ", round(hydroGOF::rmse(df[, x], df[, y]) / mean(df[, x]) * 100, 2), "%"), side = 1, line = -1)}
    if (length(eq) > 0) act::abequation(seq(par("usr")[1], par("usr")[2], 0.01), eq)
    if (reg !=  F) {
      if (is.numeric(reg)) nb.coef <- reg + 1 else nb.coef <- 2
      reg_header <- cat(paste0(paste(colnames(conditions), collapse = ";"), ";x;y;correlation;", paste(letters[1:nb.coef], collapse = ";"), "\n"))
      
      #save regression output
      if (path != F && typeof(path) != "NULL" && reg_output %in% c(2,4)) {
        filename <- file.path(if (is(try(dirname(path), T)) !=  "try-error") dirname(path) else getwd(), paste0(y, "~", x, ".csv"))
        if (new_par & cond == 1) try(file.remove(filename))
        if (cond == 1) capture.output(reg_header, file = filename, append = T)
        capture.output(cat(paste(conditions[cond, ], collapse = ";"), ";"), file = filename, append = T)
      }
      
      act::regression(df, x, y, type = reg, intercept = reg.itc, col = reg_col, output = reg_output, filename = filename, ann.pos = reg_pos, print.ann = reg_ann)
      if ((reg_output == 2 | reg_output == 4) & cond == nrow(conditions)) cat(paste("Regression data saved at", filename))

      if (second_curve) {
        if (path != F && typeof(path) != "NULL" && reg_output %in% c(2,4)) {
          filename <- file.path(if (is(try(dirname(path), T)) !=  "try-error") dirname(path) else getwd(), paste0(y2, "~", x, ".csv"))
          if (cond == 1) capture.output(reg_header, file = filename, append = T)
          capture.output(cat(paste(conditions[cond, ], collapse = ";"), ";"), file = filename, append = T)
        }
        act::regression(df2, x2, y2, type = reg, intercept = reg.itc, col = reg_col, output = reg_output, filename = filename, ann.pos = reg_pos2, print.ann = reg_ann)
      }
    }
  
    }
  }
  if (length(path) > 0 && path !=  F && opt$png_output) {
    dev.off()
    if (new_par == FALSE) warning("PNG output doesn't work well when new_par is FALSE, consider making PNG outside of graph_act and path = FALSE")
  }
}
