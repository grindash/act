#' Plot following Annals of Botany rules
#'
#' @param x character. set of data to draw
#' @param y character. set of data to draw
#' @param z character. set of data to draw
#' @param data data frame. First three columns must be the factors parameters.
#' @param LoopSize number of factor
#' @param mfrow plot number by graphical window. c(ncol, nrow)
#' @param i numeric. if i > 1 changes of the graphical window are ignored. Can have decimal to adjust plot letters if needed.
#' @param type what type of plot should be drawn. Default is points
#' @param palette 1 = black and white palette, 2 = color palette. Other palette are accepted if fully detailed : palette(new.palette)
#' @param title plot title
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param outer logical. If TRUE only one xlab and ylab are writen for all the plot. If FALSE, each plot get the xlab and ylab.
#' @param xlim Plot limit. Either c(min, max), NULL or TRUE. If TRUE all the plot will have the same value, if NULL each plot have their own value.
#' @param ylim Plot limit. Either c(min, max), NULL or TRUE. If TRUE all the plot will have the same value, if NULL each plot have their own value.
#' @param ann logical. if TRUE annotation are displayed
#' @param lg.lab a character or expression vector of length ≥ 1 to appear in the legend.
#' @param lg.pos coordinate to position the legend
#' @param lg.nb numeric. plot number where the legend should be displayed. Default is on all plots.
#' @param Mean logical. If TRUE the mean of the y is used instead of raw value.
#' @param Interval logical. If TRUE display the confidence interval of the y mean
#' @param ErrorBar logical. If TRUE display the error bar of the y mean
#' @param reg Draw the linear regression of the plot. If "nls" or "lm" return a second order regression. If numeric, return a regression from the specified order, using the lm function.
#' @param reg.itc numeric. Force the intercept of the regression
#' @param hist logical. If TRUE display the distribution of the data.
#' @param x2 second set of data to draw on the same plot
#' @param y2 second set of data to draw on the same plot
#' @param xlim2 plot parameter of the second data set
#' @param ylim2 plot parameter of the second data set
#' @param path Path where the plot are saved as png. If empty open a new window instead.
#' @param reg_pos regression position on the plot
#' @param reg_pos2 regression position on the plot
#'
#' @return a list of plot
#' @export
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @examples data(CO2)
#' @examples graph("conc", "uptake", "Type", CO2, 1, Mean = TRUE, ErrorBar = TRUE)
graph <- function(x, y, z, data, LoopSize = 3,
                     mfrow = NULL, i = 1,
                     type = "p", palette = 2, title = NULL,
                     xlab = NULL, ylab = NULL, xlim = TRUE, ylim = TRUE,
                     outer = TRUE, ann = TRUE,
                     lg.lab = "", lg.pos = "bottomright", lg.nb = c(1:1000),
                     Mean = FALSE, Interval = FALSE, ErrorBar = FALSE,
                     reg = FALSE, reg.itc = NULL, reg_pos = "left", reg_pos2 = "right",
                     hist = FALSE,
                     x2 = NULL, y2 = NULL, xlim2 = TRUE, ylim2 = TRUE,
                     path = NULL) {
  palette_backup <- palette()  # Sauvegarde la palette graphique utilisee avant la fonction

  # Modification des donnes ----
  data <- data[order(data[, 1], data[, 2], data[, 3]), ]
  data <- data[order(data[, x], decreasing = T), ]
  # data <- data[, c(3:1, 4:ncol(data))]
  data2 <- data

  if (LoopSize == 0) {
    data$loop <- 1
    data <- act::order_column(data, "loop")
  }

  if (length(levels(data[,x])) > 0) xlim <- NULL
  if (length(xlim) == 1) if (xlim == T) xlim <- c(min(na.omit(data[, x])), max(na.omit(data[, x])))
  if (length(ylim) == 1) if (ylim == T) ylim <- c(0, act::min_max(na.omit(data)[, y])[2])

  if (Mean) {
    if (LoopSize > 2) {
      warning("Mean cannot be run for Loopsize > 2, ignored")
    } else {
      options(warn = -1)
      data <- Rmisc::group.CI(as.formula(paste(y, "~", paste(colnames(data)[1:LoopSize], collapse = "+"), "+", x, "+", z)), data = data)
      options(warn = 1)
      colnames(data)[grep("mean", colnames(data))] <- y
    }
  }

  conditions <- act::unique_table(data, colnames(data)[1:LoopSize])
  if (i == 1) cat("graph performed on", colnames(conditions), "\n")

  # Fenetre graphique ----
    if (length(mfrow) == 0) if (LoopSize == 0) mfrow <- c(1, 1) else mfrow <- act::act_mfrow(data, 1:LoopSize)
    if (length(path) > 0) {
      if (path !=  F & opt$png_output & i == 1) {
        act::make_output(path)
      }
    }else if (i == 1) windows(1920, 1080)

    # if (names(dev.cur()) == "RStudioGD" | names(dev.cur()) == "null device") windows(1920, 1080)

    if (i == 1) par(mfrow = mfrow,
                    oma = rep(c(2, 2) * c(1, (1 + length(y2) / 2)), 2),  # distance autour de la zone des plots
                    mar = c(3, 4, 3, 2))  # distance entre les plots

  # Ploting loop ----
  for (cond in seq_len(nrow(conditions))) {
    act::progress_bar(cond, nrow(conditions))
    #Data
    if (Mean) {
      df <- act::act_call(data, conditions, cond)
    } else {
      df <- na.omit(act::act_call(data, conditions, cond)[, c(x, y, z, x2, y2)])
    }

    if (nrow(df) > 0) {
      #Graph properties
      if (palette == 1) palette(act::shades_of_grey(length(unique(df[, z]))))
      if (palette == 2) palette(pals::cols25(max(2, length(unique(df[, z])))))
      if (typeof(palette) == "character") palette(palette)
      pch <- rep(c(21:24), ceiling(length(unique(df[, z])) / 4))

      if (length(xlab) == 0) xlab <- x
      if (length(ylab) == 0) ylab <- y
      if (LoopSize == 1) title <- as.character(conditions[cond, ]) else if (LoopSize > 1) title <- paste(apply(conditions[cond, ], 2, as.character), collapse = " ")

      #Distribution Histogram
      if (hist == T) {
        res <- hist(df[, x], breaks = seq(xlim[1], xlim[2], (xlim[2] - xlim[1]) / 10), yaxt = "n", xaxt = "n", ann = F)
        text(x = res$mids, y = (res$counts) + (max(res$counts) / 35), labels = paste0(round(100 * res$counts / sum(res$counts)), "%"), xpd = NA)
        par(new = T)
      }

      #Plot
      plot(df[, c(x, y)], xlim = xlim, ylim = ylim, las = 1, mgp = c(3, 0.8, 0), type = "n", ylab = "", xlab = "", main = title, ann = ann) # "mgp" control the axis position: c(axis title, axis label, axis line)
      for (l in unique(df[, z])) lines(df[df[, z] == l, c(x, y)], type = type, pch = pch[grep(l, unique(df[, z]))], col = "black", bg = grep(l, unique(df[, z])))
      nbplot <- act::plot_number()

      #Annotations
      par(new = F)
      lab_letter <- paste0(rep(LETTERS, ceiling(nbplot / 26))[nbplot], if (par("mfrow")[1] * par("mfrow")[2] >= 26)ceiling(nbplot / 26.01))
      text(x = par("usr")[1], y = par("usr")[4] + (par("usr")[4] - par("usr")[3]) * 0.075, labels = lab_letter, cex = 1.5, xpd = NA)

      if (nbplot == 1 & outer == T) {
        mtext(xlab, side = 1, line = 0, cex = 0.8, outer = outer)
        mtext(ylab, side = 2, line = 0, cex = 0.8, outer = outer)
        if (length(y2) > 0) {mtext(y2, side = 4, cex = 0.8, outer = outer)}
      }else if (outer == F) {
        mtext(xlab, side = 1, line = 2, cex = 0.8, outer = outer)
        mtext(ylab, side = 2, line = 2, cex = 0.8, outer = outer)
        if (length(y2) > 0) {mtext(y2, side = 4, cex = 0.8, outer = outer)}
      }

      #Second curve
      if (length(c(x2, y2)) > 0) {
        if (length(x2) == 0) x2 <- x
        if (length(y2) == 0) y2 <- y

        if (length(xlim2) == 1) if (xlim2 == T) xlim2 <- xlim
        if (length(ylim2) == 1) if (ylim2 == T) ylim2 <- ylim

        if (Mean) {
          options(warn = -1)
          data2 <- Rmisc::group.CI(as.formula(paste(y2, "~", paste(colnames(data2)[1:LoopSize], collapse = "+"), "+", x2, "+", z)), data = data2)
          options(warn = 1)
          colnames(data2)[grep("mean", colnames(data2))] <- y2
          df2 <- act::act_call(data2, conditions, cond)
        } else {
          df2 <- df
        }

        par(new = T)
        plot(df2[, x2], df2[, y2], xlim = xlim2, ylim = ylim2, type = type, pch = pch[grep(i, unique(df[, z]))], col = "black", bg = grep(i, unique(df[, z]))+1, ann = F, xaxt = "n", yaxt = "n", bty = "n")
        if (length(intersect(xlim, xlim2)) !=  2) {axis(side = 3, las = 1, labels = T)}
        if (length(intersect(ylim, ylim2)) !=  2) {axis(side = 4, las = 1, labels = T)}}

      #Legend
      if (nbplot %in% lg.nb) {
        if (length(c(x2, y2)) > 0) {
          lg.txt <- c(y, y2)
          pch <- rep(pch[grep(i, unique(df[, z]))], 2)
        } else {
          lg.txt <- unique(df[, z])
        }
        pt.bg <- c(seq_len(length(pch)))
        nb.pch <- c(seq_len(length(pch)))

        if (length(lg.pos) == 1) {lg.x <- lg.pos; lg.y <- NULL}
        if (length(lg.pos) == 2) {lg.x <- lg.pos[1] * par("usr")[2]; lg.y <- lg.pos[2] * par("usr")[4]}
        legend(lg.x, lg.y, lg.txt, title = lg.lab, col = "black", pt.bg = pt.bg, pch = pch[nb.pch], bty = "n", ncol = ceiling(length(lg.txt) / 5))
      }

      #Confidence Interval
      if (Interval) {
        if (Mean) {
          for (ech in seq_len(length(unique(df[, z])))) {
            conf_int <- df[df[, z] == unique(df[, z])[ech], grep(paste(c(x, "lower", y, "upper"), collapse = "|"), colnames(df))]
            conf_int[is.nan(conf_int[, 2]), 2] <- conf_int[is.nan(conf_int[, 2]), 3] #Remove NaN value preventing the polygon to correctly appear
            conf_int[is.nan(conf_int[, 4]), 4] <- conf_int[is.nan(conf_int[, 4]), 3]
            ICcol <- rgb(t(col2rgb(palette())), alpha = ceiling(0.3 * 255), maxColorValue = 255)  # alpha set the color opacity
            polygon(c(conf_int[, x], rev(conf_int[, x])), c(conf_int[, grep("lower", colnames(conf_int))], rev(conf_int[, grep("upper", colnames(conf_int))])), col = ICcol[ech], border = "black")
          }
        } else {warning("Mean must be defined as TRUE if you want Confidence Interval")}
      }

      #Error Bars
      if (ErrorBar) {
        if (Mean) {
          arrows(x0 = df[, x], x1 = df[, x], y0 = df[, grep("upper", colnames(df))], y1 = df[, grep("lower", colnames(df))], length = 0.05, angle = 90, code = 3)
        } else warning("Mean must be defined as TRUE if you want error bars")
      }

      #Regression
      if (reg !=  F) {
        if (typeof(path) == "character") {
          filename <- file.path(if (class(try(dirname(path), T)) !=  "try-error") {dirname(path)} else {getwd()}, paste0(y, "~", x, ".csv"))
          if (is.numeric(reg)) nb.coef <- reg + 1 else nb.coef <- 2
          if (cond == 1) capture.output(cat(paste0(paste(colnames(conditions), collapse = ";"), ";x;y;correlation;", paste(letters[1:nb.coef], collapse = ";"), "\n")), file = filename, append = T)
          capture.output(cat(paste(conditions[cond, ], collapse = ";"), ";"), file = filename, append = T)
          output.reg <- 3
        } else output.reg <- 1
        act::regression(df, x, y, type = reg, intercept = reg.itc, output = output.reg, filename = filename, ann.pos = reg_pos)

        if (length(y2) > 0) {
          if (typeof(path) == "character") {
            filename <- file.path(if (class(try(dirname(path), T)) !=  "try-error") {dirname(path)} else {getwd()}, paste0(y2, "~", x, ".csv"))
            if (is.numeric(reg)) nb.coef <- reg + 1 else nb.coef <- 2
            if (cond == 1) capture.output(cat(paste0(paste(colnames(conditions), collapse = ";"), ";x;y;correlation;", paste(letters[1:nb.coef], collapse = ";"), "\n")), file = filename, append = T)
            capture.output(cat(paste(conditions[cond, ], collapse = ";"), ";"), file = filename, append = T)
          } else output.reg <- 1
          act::regression(df2, x2, y2, type = reg, intercept = reg.itc, output = output.reg, filename = filename, ann.pos = reg_pos2)
        }
      }
    }
  }
  if (length(path) > 0) if (path !=  F) if (opt$png_output) {
    graphics.off()
    if (i > 1) warning("PNG output doesn't work well when i > 1, consider making PNG outside of graph_act and path = FALSE")
  }
  palette(palette_backup)
}
