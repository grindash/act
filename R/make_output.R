#' Create png with predetermined options
#'
#' @param file file name output
#'
#' @return open a png file, must be closed afterward
#' @export
make_output <- function(file) {
  if (exists("opt", envir = .GlobalEnv)) {
    opt <- get("opt", envir = .GlobalEnv)
  } else {
    opt <- list(ann = TRUE, pal = 2, res = 600, png_output = TRUE, plot_size = 4)
  }
  par_save <- par()[grep("cin|cra|csi|cxy|din|page|pin|new", names(par()), invert = T)]

  for (chr in c(':', '?', '*', '<', '>', '|', '"')) {
    file_name <- unlist(strsplit(file, "/"))[length(unlist(strsplit(file, "/")))]
    if (length(grep(chr, file_name, fixed = T)) > 0) {
      warning("'", chr, "' is not allowed for filenames. Deleted from string.")
      file_name <- gsub(chr, "", file_name)
      file <- paste(paste(unlist(strsplit(file_name, "/"))[1:(length(unlist(strsplit(file_name, "/")))-1)], collapse = "/"), file_name, sep = "/")
    }
  }

  if (opt$png_output) {
    if (dirname(file) != ".") dir.create(file.path(dirname(file)), showWarnings = F, recursive = T) # Create folder if non existing
    if (par("mfrow")[1] * par("mfrow")[2] <= 2) png(file, 800 * par("mfrow")[2], 800 * par("mfrow")[1], res = 100)
    if (par("mfrow")[1] * par("mfrow")[2] > 2) png(file, opt$plot_size * par("mfrow")[2], opt$plot_size * par("mfrow")[1], "in", res = opt$res)
    par(par_save)
  } else {
    windows(1920, 1080)
    par(par_save)
  }
}
