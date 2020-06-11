#' create a basic progression bar
#' May lag if total if really high.
#'
#' @param i iteration
#' @param total total number of iteration
#'
#' @return progress bar in console
#' @export
#'
#' @examples for (i in 1:10) {Sys.sleep(0.1); progress_bar(i, 10)}
progress_bar <- function(i, total) {
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  setTxtProgressBar(pb, i)
  if (i %% total == 0) {
    close(pb)
  }
}
