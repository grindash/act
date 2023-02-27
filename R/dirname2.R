#' Dirname with a loop
#'
#' @param path path
#' @param nb number of directory to pass
#' @param pattern pattern to find
#'
#' @return path
#' @export
dirname2 <- function(path, nb=1, pattern=NULL) {
  og_path <- path
  if (is.null(pattern)) {
    for (i in 1:nb) path <- dirname(path)
  } else {
    not_found <- TRUE
    while (not_found) {
      if (path == dirname(path)) stop(paste0("Pattern '", pattern, "' not found in ", og_path))
      path <- dirname(path)
      if (basename(path) == pattern) not_found <- FALSE
    }
  }
  return(path)
}
