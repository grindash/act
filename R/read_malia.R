#' Lis les sorties de CARIBU
#'
#' @param path chemin vers le csv
#' @param manip nom de l'experimentation
#' @param manip_data jsp en vrey
#'
#' @return list or data.frame
#' @export
#' 
#' @importFrom stringr str_split_fixed
read_malia <- function(path, manip=NULL, manip_data=NULL) {
  if (is.character(path)) data <- read.csv(path, sep = ";", header = T) else data <- path
  
  # Verification des donnees
  if (nrow(data[!is.na(data$zeta_malia), ]) == 0) stop("la collone 'zeta_malia' ne contient que des NA")
  if (is.null(data$sky)) data$sky <- "soc"
  if (is.null(manip)) manip <- tolower(unique(data$expe)) else manip <- tolower(manip)
  if (!"reconstruction" %in% colnames(data)) data$reconstruction <- act::get_name_info(path, "mc|bp")
  
  if (manip == "archiduo") {
    cat("ArchiDuo\n")
    info_semis <- stringr::str_split_fixed(data$id, "_", 3)
    data$orientation <- info_semis[, 3]
    data <- act::order_column(data, c("date", "bloc", "distance", "orientation"))
    nom_maya <- c("bloc", "distance", "orientation")
    
    to_return <- list('data' = data, 'nom_maya' = nom_maya)
  } else if (manip == "archimono") {
    cat("ArchiMono\n")
    data$distance <- as.numeric(data$distance)
    info_semis <- stringr::str_split_fixed(data$bloc, "", 2)
    colnames(data)[grep("bloc", colnames(data))] <- "semis"
    data$rep <- stringr::str_split_fixed(data$id, "-", 2)[, 2]
    data$ray <- "direct"
    
    if (!is.null(manip_data)) {
      manip_data$distance <- as.numeric(manip_data$distance)
      data <- plyr::join(data, manip_data, by = c("date", "semis", "rep", "distance"))
    }

    nom_maya <- c("semis", "rep", "distance")
    
    to_return <- list('data' = data, 'nom_maya' = nom_maya)
  } else {
    stop("Manip non identifiee\n")
  }
  
  return(to_return)
}
