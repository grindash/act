#' Lis les sorties de CARIBU
#'
#' @param path chemin vers le csv
#' @param manip nom de l'experimentation
#'
#' @return list or data.frame
#' @export
#' 
#' @importFrom stringr str_split_fixed
read_caribu <- function(path, manip=NULL) {
  if (is.character(path)) df_caribu <- read.csv(path, sep = ";", header = T) else df_caribu <- path
  if (is.null(df_caribu$sky)) df_caribu$sky <- "soc"
  
  if (is.null(manip)) {
    if (!is.na(unique(df_caribu$semis)[1])) {
      check_manip <- length(unlist(strsplit(df_caribu$semis[1], "_"))) == 3
      if (check_manip) manip <- "archiduo" else manip <- "archimono"
    } else {
      manip <- "NA"
    }
  } else {
    manip <- tolower(manip)
  }
  
  if (manip == "archiduo") {
    cat("ArchiDuo\n")
    colnames(df_caribu)[grep("rep", colnames(df_caribu))] <- "bloc"
    info_semis <- stringr::str_split_fixed(df_caribu$semis, "_", 3)
    df_caribu$distance <- as.numeric(info_semis[, 2])
    df_caribu$orientation <- info_semis[, 3]
    df_caribu <- act::order_column(df_caribu, c("date", "heure", "semis", "bloc", "distance", "orientation"))
    nom_maya <- c("bloc", "distance", "orientation")
    
    to_return <- list('df' = df_caribu, 'nom_maya' = nom_maya)
  } else if (manip == "archimono") {
    cat("ArchiMono\n")
    df_caribu$distance <- as.numeric(df_caribu$distance) / 10
    nom_maya <- c("semis", "rep", "distance")
    
    to_return <- list('df' = df_caribu, 'nom_maya' = nom_maya)
  } else {
    cat("Manip non identifiee\n")
    df_caribu$distance <- 100
    to_return <- df_caribu
  }
  
  return(to_return)
}
