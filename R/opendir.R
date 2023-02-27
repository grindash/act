#' Ouvre un dossier windows depuis Rstudio
#'
#' @param dir ouvre le dossier du chemin. defaut: repertoire de travail
#'
#' @return path
#' @export
opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}
