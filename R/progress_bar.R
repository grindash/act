#' create a basic progression bar
#' May lag if total is really high.
#'
#' @param i iteration
#' @param total total number of iteration
#' @param title text before the progress bar
#' @param label text after the progress bar
#' @param start_time time at the first iteration
#' @param time_units secs, mins or hours
#' @param ... additionnal arg
#'
#' @return progress bar in console
#' @export
#' 
#' @import utils
progress_bar <- function(i, total, title = "", label = "", start_time = NULL, time_units = "secs", ...) {
  if (length(i) > 1) {
    warning("Plusieurs valeurs de i detectees, utilisation de la derniere valeur uniquement")
    i <- i[length(i)]
  }
  if (total < i) stop("must have 'total' >= 'i'")
  
  #time
  if (is.null(start_time)) {
    if (i == 1) {
      start_time <<- Sys.time()
      # start_time <- Sys.time()
      # assign("start_time", start_time, envir = .GlobalEnv)
    } else {
      if (exists("start_time", envir = .GlobalEnv)) start_time <- get("start_time", envir = .GlobalEnv)
    }
  }

  #progress bar function
  txtPB <- function(min = 0, max = 1, initial = 0, char = "=", width = NA, title = "", label = "") {
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
      pc <- floor(100 * (value - min) / (max - min))
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

  #timer
  if ((!is.null(start_time)) && start_time != FALSE) {
    progress <- i / total
    run_time <- as.numeric(difftime(Sys.time(), start_time, units = time_units))
    if (progress != 1) {
      remaining_time <- floor((run_time / progress) - run_time)
      if (time_units == "secs") {
        d <- remaining_time %/% (3600 * 24)
        h <- remaining_time %/% 3600
        m <- (remaining_time - h * 3600) %/% 60
        s <- remaining_time - (h * 3600 + m * 60)
        label <- paste0("- ", if (h > 0) paste0(h, "h "), if (m > 0) paste0(m, "min "), paste0(s, "sec "), "remaining")
      }
      else label <- paste("-", round(remaining_time, 2), time_units, "remaining")
    } else {
      run_time <- floor(run_time)
      h <- run_time %/% 3600
      m <- (run_time - h * 3600) %/% 60
      s <- run_time - (h * 3600 + m * 60)
      label <- paste0("- ", "Ended in ", if (h > 0) paste0(h, "h "), if (m > 0) paste0(m, "min "), paste0(s, "sec"))
    }
  }

  #progress bar printing
  pb <- txtPB(min = 0, max = total, title = title, label = label, ...)
  utils::setTxtProgressBar(pb, i)
  if (i %% total == 0) {
    close(pb)
    if (!is.null(start_time) && start_time != FALSE) rm(start_time, envir = .GlobalEnv)
  }
}
