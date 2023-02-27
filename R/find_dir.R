#' List the directories with a given pattern
#'
#' @param basedir Base directory in which look for
#' @param pattern Only directory names which match the regular expression will be returned.
#' @param ignore ignore les chemins de dossier possedant ces patterns
#'
#' @return list of dirs
#' @export
find_dir <- function(basedir, pattern, ignore=NULL) {
  ignore <- c(ignore, ".stversions", ".stfolder")
  all_dir <- list.dirs(basedir, recursive = T)
  all_dir <- grep(paste(ignore, collapse = "|"), all_dir, invert = T, value = T)
  for (ptrn in pattern) {
    basedir <- all_dir[grep(ptrn, basename(all_dir))]
  }
  return(basedir)
}
