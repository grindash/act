#' Turn windows path into usable one.
#' No argument needed.
#'
#' @return path the clipboard
#' @export
repath <- function() {
   cat('Paste windows file path and hit RETURN')
   x <- scan(what = "", quiet = TRUE)
   xa <- gsub('\\\\', '/', x)
   xa <- paste(xa, collapse = " ")
   xa <- paste0('"', xa, '"')
   writeClipboard(xa)
   cat("Here's your de-windowsified path. (It's also on the clipboard.)\n", xa, "\n")
}
