#' Create png with predetermined options. Warning : par() modifications must be done before using this function or they won't be taken into account.
#'
#' @param file file name output
#' @param format png ou svg
#' @param size size of the outplut in pixels
#' @param screen_ratio character or numeric. change the screen ratio, default is 1:1
#' @param envir environment to grab opt
#' @param ... additionnal par() arguments 
#'
#' @return open a png file, must be closed afterward
#' @export
#' 
#' @import stringr
#' @importFrom tools file_ext
make_output <- function(file, format, size = 800, screen_ratio = "1:1", envir = .GlobalEnv, ...) {
  if (exists("opt", envir = envir)) {
    opt <- get("opt", envir = envir)
  } else {
    opt <- act::default_opt()
  }
  if (missing(file)) {
    opt$png_output <- FALSE
    file <- NULL
  }
  
  if (missing(format)) format <- "png"

  par(...)

  if (opt$png_output) {
    par_save <- par()[grep("cin|cra|csi|cxy|din|page|pin|new|plt", names(par()), invert = T)]
    # Check path
    if (tools::file_ext(file) %in% c("png", "svg", "pdf")) format <- tools::file_ext(file) else file <- paste0(file, ".", format)
    for (chr in c(':', '?', '*', '<', '>', '|', '"')) {
      file_name <- basename(file)
      if (length(grep(chr, file_name, fixed = T)) > 0) {
        warning("'", chr, "' is not allowed for filenames. Deleted from string.")
        file_name <- gsub(chr, "", file_name)
        file <- paste(paste(unlist(strsplit(file_name, "/"))[1:(length(unlist(strsplit(file_name, "/"))) - 1)], collapse = "/"), file_name, sep = "/")
      }
    }
    if (dirname(file) != ".") dir.create(file.path(dirname(file)), showWarnings = F, recursive = T) # Create folder if non existing
    
    # Create file output
    screen_ratio <- eval(parse(text = gsub(":", "/", screen_ratio)))
    
    if (format == "png") {
      nb_plot <- par("mfrow")[1] * par("mfrow")[2]
      size <- if (nb_plot <= 2) 6 else opt$plot_size
      width <- screen_ratio * size * par("mfrow")[2] * if (nb_plot <= 2) 1.2 else 1
      height <- size * par("mfrow")[1] * if (nb_plot <= 2) 1.2 else 1
      png(file, width, height, units = "in", res = opt$res)
    }
    
    if (format == "svg") {
      svg_res <- 150
      width <- screen_ratio * size / svg_res * par("mfrow")[2]
      height <- size / svg_res * par("mfrow")[2]
      svg(file, width, height)
    }
    
    if (format == "pdf") {
      size <- 3.5
      width <- screen_ratio * size * par("mfrow")[2]
      height <- size * par("mfrow")[1]
      pdf(file, width, height)
    }
    
    par(par_save)
    par(new = F)
  }
}
