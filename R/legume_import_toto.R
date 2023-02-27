#' Importe un fichier "toto" de L-egume et le converti en data.frame avec les parametres par colonne
#'
#' @param toto_list liste des fichier toto
#' @param zip_path path de l'archive
#'
#' @return data frame
#' @export
legume_import_toto <- function(toto_list, zip_path) {
  toto_full <- data.frame()
  for (toto_path in toto_list) {
    act::progress_bar(grep(toto_path, toto_list), length(toto_list), "toto import")
    toto <- read.csv(unz(zip_path, toto_path), sep = ";", header = T)
    toto <- toto[toto$V1 != "pattern", ]
    
    plt <- NULL
    for (i in seq_len(length(grep(colnames(toto)[3], colnames(toto))))) plt[i] <- i  # potentiel futur bug pour d'autre conditions
    if (length(plt) != (ncol(toto) - 2)) stop("Ce script fonctionne pour 1 seule plante actuellement, a mettre a jour")
    variete <- stringr::str_split_fixed(colnames(toto), "\\.", 2)[3:ncol(toto), 1]
    colnames(toto) <- c("V1", "steps", plt)
    
    toto <- data.frame(t(reshape(toto, idvar = "V1",
                                 v.names = colnames(toto)[-c(1:2)],
                                 timevar = "steps", direction = "wide")))
    
    infos_sup <- stringr::str_split_fixed(row.names(toto), "\\.", 2)
    toto$nump <- c("nump", infos_sup[-1, 1])
    toto$DOY <- c("DOY", infos_sup[-1, 2])
    toto <- act::read_csv_clean(toto, header = F)
    
    #recuperation de l'id de l'usm
    infos_path <- unlist(strsplit(toto_path, "_"))
    toto$ID_usm <- infos_path[grep("l-egume", infos_path) - 1]  # Quelque soit les simulations l'id des usm est situe avant le nom du logiciel
    for (i in seq_len(ncol(toto))) toto[, i] <- as.numeric(as.character(toto[, i]))
    # colnames(toto)[grep("TT", colnames(toto))] <- "TT_toto"
    toto_full <- rbind(toto_full, toto)
  }
  rm(toto_list, zip_path, toto_path, plt, i, variete, infos_sup, infos_path)
  return(toto_full)
}