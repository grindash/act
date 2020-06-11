#' Compute and draw the Linear Regression on a plot
#'
#' @param df data frame
#' @param x x parameter
#' @param y y parameter
#' @param type If "nls" or "lm" return a second order regression. If numeric, return a regression from the specified order, using the lm function ("lm" return the same results than 1).
#' @param intercept numeric, force the intercept value
#' @param output 1 draws a regression onto the current plot. 2 writes the regression parameters in a data frame. 3 do both.
#' @param filename output filename. csv format by default
#' @param ann.pos equation position
#'
#' @return draw a curve on a pre-existing plot
#' @export
#'
#' @importFrom stats nls lm
#' @import graphics
regression <- function(df, x, y, type, intercept = NULL, output = 1, filename = paste0("regression_", Sys.Date(), ".csv"), ann.pos = "right") {
  if (output %in% c(1:3)  ==  FALSE) {
    warning("'output' must be 1, 2 or 3. 1 draws a regression onto the current plot. 2 writes the regression parameters in a data frame. 3 do both.\n")
  }
  output <- switch(output, "1" = "plot", "2" = "table", "3" = c("plot", "table"))

  if (length(intercept) > 1) {
    warning("intercept length must be 1, first value kept for regression")
    intercept <- intercept[[1]]
  }

  #Nonlinear Least Squares
  if (type == "nls") {
    if (is.numeric(intercept)) {
      modele <- nls(sprintf("%s ~ a * %s + %s", y, x, intercept), df, start = c(a = 1))
    } else {
      modele <- nls(sprintf("%s ~ a * %s + b", y, x), df, start = c(a = 1, b = 1))
    }
    coef <- c(coef(modele)[2], coef(modele)[1])
    if (length(output[output == "plot"]) > 0) abline(coef, lwd = 3, col = "grey50")
  }

  #Linear Models
  if (type == "lm") type <- 1
  if (is.numeric(type)) {
    if (is.numeric(intercept)) {
      modele <- lm(I(df[, y] - intercept) ~ poly(df[, x], type, raw = T) - 1)
    } else {
      modele <- lm(df[, y] ~ poly(df[, x], type, raw = T))
    }
    coef <- na.omit(c(intercept, coef(modele)))
    if (length(output[output == "plot"]) > 0) lines(df[, x], predict(modele) + if (is.numeric(intercept)) intercept else 0, lwd = 3, col = "grey50")
  }

  #Output
  if (exists("modele")) {
    #Equation on the plot
    if (length(output[output == "plot"]) > 0) {
      equation <- "y  = "
      for (i in rev(seq_len(length(coef)))) {
        equation <- paste0(equation, " ", round(coef[i], 2))
        if (i > 1) {
          if (sign(coef[i - 1]) >= 0) sign <- " +" else sign <- " "
          equation <- paste0(equation, "x^", i - 1, sign)
        }
        equation <- gsub("x^1", "x", equation, fixed = T)
      }

      x_pos <- switch(ann.pos,
                      right = par("usr")[2],
                      bottomright = par("usr")[2],
                      left = par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.05,
                      bottomleft = par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.05)

      y_pos <- switch(ann.pos,
                      right = par("usr")[4],
                      bottomright = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.05,
                      left = par("usr")[4],
                      bottomleft = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.05)

      adj_pos <- switch(ann.pos, right = 1, bottomright = 1, left = 0, bottomleft = 0)

      r_square <- substitute(paste(R^2, " = ", rsquare, " "), list(rsquare = round(cor(df[, y], predict(modele))^2, 3)))

      options(warn = -1)
      text(x = x_pos, y = y_pos + (par("usr")[4] - par("usr")[3]) * 0.05, labels = r_square, adj = adj_pos, xpd = NA)
      text(x = x_pos, y = y_pos - (y_pos - par("usr")[3]) * 0.05, labels = equation, adj = adj_pos, xpd = NA)
      options(warn = -1)
    }

    #Equation saved in data frame
    if (length(output[output == "table"]) > 0) {
      output_model <- paste(x, y, round(cor(df[, y], predict(modele))^2, 3), sep = ";")
      for (i in rev(seq_len(length(coef)))) {
        output_model <- paste(output_model, paste(round(coef[i], 2), collapse = ";"), sep = ";")
      }
      capture.output(cat(output_model, "\n"), file = filename, append = T)
    }
  } else warning('Regression must be "nls", "lm", or numeric (degree)')
}
