#' create a basic progression bar
#' May lag if total is really high.
#'
#' @param i iteration
#' @param total total number of iteration
#' @param title text before the progress bar
#' @param label text after the progress bar
#'
#' @return progress bar in console
#' @export
#'
#' @examples for (i in 1:10) {Sys.sleep(0.1); progress_bar(i, 10)}
progress_bar <- function(i, total, title = "", label = "") {
  if (total < i) stop("must have 'total' >= 'i'")

  txtPB<- function (min = 0, max = 1, initial = 0, char = "=", width = NA, title = "", label = "") {
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (is.na(width)) {
      width <- getOption("width") - (10L + nchar(title, "w") + nchar(label, "w"))
      width <- trunc(width / nw)
    }
    
    up <- function(value) {
      if (!is.finite(value) || value < min || value > max) return()
      .val <<- value
      nb <- round(width * (value - min) / (max - min))
      pc <- round(100 * (value - min) / (max - min))
      if (nb == .nb && pc == .pc) return()
      cat(paste0("\r ", title," |", strrep(" ", nw * width + 6)))
      cat(paste(c("\r ", title," |", rep.int(char, nb), rep.int(" ", nw * (width - nb)), sprintf("| %3d%% ", pc), label), collapse = ""))
      flush.console()
      .nb <<- nb
      .pc <<- pc
    }
    
    getVal <- function() .val
    kill <- function() if (!.killed) {
      cat("\n")
      flush.console()
      .killed <<- TRUE
    }
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
  }
  
  
  pb <- txtPB(min = 0, max = total, title = title, label = label)
  utils::setTxtProgressBar(pb, i)
  if (i %% total == 0) {
    close(pb)
  }
}
