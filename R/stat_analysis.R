#' Make multiple statistical analysis on a data set
#' lm(Y ~ factors, data)
#'
#' @param data data set
#' @param Y Variable
#' @param factors Factors
#' @param sep Multiple loop condition
#' @param SavePath If fill, save all the analysis into the indicated folder
#' @param test Choose the test to effectuate. 1 = ANOVA, 2 = TukeyHSD, 3 = BargroupHSD, 4 = ANOVA2. Default is c(1:4).
#' @param l Add letters to plot if TRUE.
#' @param eb Add errorbars to plot if TRUE.
#' @param mfrow plot number
#'
#' @return ANOVA, TukeyHSD, Bargroup HSD,
#' @export
#'
#' @import ggplot2
#' @import car
#' @import stats
#' @import agricolae
#' @import multcomp
#'
#' @examples data("CO2")
#' @examples stat_analysis(CO2, "uptake", c("Treatment", "Type"))
stat_analysis <- function(data, Y, factors, sep = NULL, test = c(1:4), mfrow = NULL, SavePath = NULL, l = F, eb = F){
  options(warn = 1)
  if (length(sep) == 0) {
    data$no_sep  <- " "
    sep <- "no_sep"
    warning_txt <- "No 'sep' define."
    if (length(grep(2, test)) > 0) {
      test <- test[grep(2, test, invert = T)]
      warning_txt <- paste(warning_txt, "TukeyHSD cannot be performed.")
    }
    if (length(grep(4, test)) > 0) {
      test <- test[grep(4, test, invert = T)]
      warning_txt <- paste(warning_txt, "ANOVA2 cannot be performed.")
    }
    warning(warning_txt)
  }
  data[factors] <- lapply(data[factors], as.factor)
  list_sep <- act::unique_table(data, sep)

  for (conditions in seq_len(nrow(list_sep))) {
    param <- paste(c(Y, list_sep[conditions, ]), collapse = " ")
    if (exists("percent")) percent <- percent + 1 else percent <- 1
    cat(param, "\n")

    df <- act::act_call(data, list_sep, conditions)

    #Anova
    if (length(grep(1, test)) == 1) {
      cat("\tANOVA\n")
      if (length(SavePath > 0)) dir.create(file.path(SavePath, "ANOVA", Y), recursive = T, showWarnings = F)
      modele <- lm(as.formula(paste(Y, "~", paste(factors, collapse = " +"))), data = df)
      if (length(SavePath > 0)) {
        capture.output(car::Anova(modele), file = file.path(SavePath, "ANOVA", Y, paste0("ANOVA ", param, ".txt")))
      }else{
        print(car::Anova(modele))
        cat("\n")
      }
    }

    #Test post-hoc - Comparaison multipleS
    if (length(grep(2, test)) == 1) {
      cat("\tTukeyHSD\n")
      if (length(SavePath > 0)) {
        dir.create(file.path(SavePath, "TukeyHSD", Y), recursive = T, showWarnings = F)
        pdf(file.path(SavePath, "TukeyHSD", Y, paste0("TukeyHSD ", param, ".pdf")), 15, 7)
      } else windows(record = T)
      aov_modele <- aov(lm(as.formula(paste(Y, " ~ ", paste(factors, collapse = " * "))), data = df)) #meme modele qu'avec lm, aov est mieux gere par agricolae
      comp_hsd <- stats::TukeyHSD(aov_modele)
      par(oma = c(0, 25, 0, 0))
      plot(comp_hsd, las = 1)
      if (length(SavePath > 0)) graphics.off()
    }

    if (length(grep(3, test)) == 1) {
      cat("\tBarGroup HSD\n")
      if (length(SavePath > 0)) {
        if (length(mfrow) == 0) mfrow <- act::act_mfrow(factors, 1) else mfrow <- mfrow
        dir.create(file.path(SavePath, "BarGroup HSD", Y), recursive = T, showWarnings = F)
        png(file.path(SavePath, "BarGroup HSD", Y, paste0("BarGroup HSD ", param, ".png")), 5 * mfrow[2], 3.5 * mfrow[1], res = 400, units = "in")
        par(mar = c(3, 5, 3, 0))
        par(mfrow = mfrow)
      } else if (length(mfrow) != 0) par(mfrow = mfrow)

      aov_modele <- aov(lm(as.formula(paste(Y, " ~ ", paste(factors, collapse = " * "))), data = df)) #même modele qu'avec lm, aov est mieux gere par agricolae
      df_res <- df.residual(aov_modele) #degre de liberte des residues
      ms_err <- deviance(aov_modele) / df_res #erreur des carres moyens

      for (i in factors) {
        comp_hsd_agri <- agricolae::HSD.test(df[, Y], df[, i], df_res, ms_err)
        comp_hsd_grp <- comp_hsd_agri$groups[levels(df[, i]), ]
        comp_hsd_std <- comp_hsd_agri$means[levels(df[, i]), c("std")]
        comp_hsd <- cbind(comp_hsd_grp, comp_hsd_std)
        row.names(comp_hsd) <- row.names(comp_hsd_grp)
        bar_group <- agricolae::bar.group(comp_hsd, ylim = c(0, max(na.omit(df[, Y])) * 1.15),
                                          xlab = "", ylab = "", yaxt = "n", cex.names = if (length(factors) == 1) 0.8 else 1,
                                          main = paste(strsplit(param, " ")[[1]][-1], collapse = " "))
        axis(2, las = 1, labels = T)
        if (eb == T) arrows(x0 = bar_group$x, x1 = bar_group$x, y0 = comp_hsd[["df[, Y]"]] - comp_hsd[["comp_hsd_std"]], y1 = comp_hsd[["df[, Y]"]] + comp_hsd[["comp_hsd_std"]], length = 0.05, angle = 90, code = 3)

        if (length(grep("plt.m-2", comp_hsd$name)) > 0) {
          comp_hsd$name <- as.factor(rownames(comp_hsd))
          comp_hsd$name <- relevel(comp_hsd$name, "11plt.m-2")
          comp_hsd <- comp_hsd[order(comp_hsd$name), ]
        }
        text(x = bar_group$x, y = comp_hsd[[1]] - 0.05 * max(na.omit(df[, Y])), labels = act::trunc2(comp_hsd[[1]], 3))
        if (l == T) text(par("usr")[1], par("usr")[4] * 1.05, labels = LETTERS[conditions], xpd = NA)
      }
      if (length(SavePath > 0)) graphics.off()
    }

    if (length(grep(4, test)) == 1) {
      cat("\tBoxplot ANOVA2\n")
      if (length(factors) > 1) {
        for (comb in seq_len(ncol(combn(factors, 2)))) {
          if (length(SavePath > 0)) {
            dir.create(file.path(SavePath, "ANOVA2", Y), recursive = T, showWarnings = F)
            png(file.path(SavePath, "ANOVA2", Y, paste0("Boxplot ANOVA2 ", param, " ", paste(combn(factors, 2)[, comb], collapse = " "), ".png")), 10, 7, res = 400, units = "in")
            par(mfrow = mfrow)
          }

          df$comb_fac <- as.factor(paste(df[, combn(factors, 2)[1, comb]], df[, combn(factors, 2)[2, comb]], sep = "_"))
          options(warn = -1)
          mc_tukey <- multcomp::glht(lm(df[, Y] ~ comb_fac, data = df), linfct = multcomp::mcp(comb_fac = "Tukey"))
          tuk_cld <- multcomp::cld(mc_tukey)
          print(tuk_cld)
          stop()
          options(warn = 1)
          myletters_df <- data.frame(Echelle = t(as.data.frame(strsplit(levels(df$comb_fac), "_")))[, 2], densite = t(as.data.frame(strsplit(levels(df$comb_fac), "_")))[, 1], letters = tuk_cld$mcletters$Letters)
          if (length(grep("plt.m-2", myletters_df$densite)) > 0) myletters_df$densite <- relevel(myletters_df$densite, "11plt.m-2")
          myletters_df <- myletters_df[order(myletters_df[, 1], myletters_df[, 2]), ]

          plot <- (ggplot2::ggplot(df, ggplot2::aes(x = df[, combn(factors, 2)[2, comb]], y = df[, Y],  fill = df[, combn(factors, 2)[1, comb]])) +
                    ggplot2::geom_boxplot(alpha = 0.5) +
                    ggplot2::scale_colour_manual(values = act::shades_of_grey(length(levels(df[, combn(factors, 2)[1, comb]])))) +
                    ggplot2::scale_fill_manual(values = act::shades_of_grey(length(levels(df[, combn(factors, 2)[1, comb]])))) +
                    ggplot2::labs(x = combn(factors, 2)[2, comb], y = Y, fill = combn(factors, 2)[1, comb]) +
                    ggplot2::theme_classic()
          )
          plot <- plot + ggplot2::annotate("text", x = ggplot2::ggplot_build(plot)$data[[1]]$x, y = max(df[, Y]) * 1.05, label = as.character(myletters_df$letters)) +
                         ggplot2::annotate("text", x = ggplot2::ggplot_build(plot)$data[[1]]$x, y = max(df[, Y]) * 1.05, label = as.character(myletters_df$letters))

          #Multiple plot with ggplot2
          ggplot_print(plot)
          if (length(SavePath > 0)) graphics.off()
        }
      } else warning("ANOVA2 cannot be run, only 1 factor given")
    }
  }
}
