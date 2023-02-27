#' Donne le format de date
#'
#' @param datetime data
#'
#' @return str
#' @export
get_date_format <- function(datetime) {
  datetime <- datetime[1]
  
  # Format date
  d_pos <- grep(":", unlist(strsplit(datetime, ' ')), invert = T)
  if (length(d_pos) > 0) {
    d <- grep(":", unlist(strsplit(datetime, ' ')), value = T, invert = T)
    d_separator <- unlist(strsplit(gsub("\\d+", "", d), "(?!^|$)", perl = T))
    d_split <- unlist(strsplit(d, paste(d_separator, collapse = "|")))
    
    year_pos <- which(nchar(d_split) == 4)
    if (year_pos == 1) format_d <- paste0("%Y", d_separator[1], "%m", d_separator[2], "%d")
    if (year_pos == 3) format_d <- paste0("%d", d_separator[1], "%m", d_separator[2], "%Y")
    
    # A faire : ajouter un verif pour la position du mois
  } else {
    format_d <- NULL
  }
  
  # Format heure
  h_pos <- grep(":", unlist(strsplit(datetime, ' ')), invert = F)
  if (length(h_pos) > 0) {
    h <- grep(":", unlist(strsplit(datetime, ' ')), value = T, invert = F)
    format_h <- paste(c("%H", "%M", "%S")[1:length(unlist(strsplit(h, ":")))], collapse = ":")
  } else {
    format_h <- NULL
  }
  
  # Fusion date et heure
  if (length(d_pos) > 0) {
    if (is.null(format_h)) {
      datetime_format <- format_d
    } else {
      if (d_pos == 1) datetime_format <- paste(format_d, format_h)
      if (d_pos == 2) datetime_format <- paste(format_h, format_d)
    }
  } else {
    datetime_format <- format_h
  }
  
  return(datetime_format)
}