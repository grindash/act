#' Importe l'ensemble des sorties de L-egume
#'
#' @param path chemin de l-egume
#' @param dates liste des simulations a agglomerer
#' @param compar logical. si TRUE ajoute un data frame de comparaison en sorties
#' @param pos_org_vox logical. si TRUE recupere la position des organes par rapport a la face inferieur du voxel
#' @param mature logical. si TRUE seul les donnees jusqu'a maturite sont recuperees
#' @param dir_multisim creation ou non d'un repertoire commun si plusieurs dates
#'
#' @return data frame
#' @export
#'
#' @importFrom readxl read_xls
#' @importFrom plyr join
legume_import <- function(path, dates, compar=F, pos_org_vox=F, mature=T, dir_multisim=F) {
  # if (!grepl('multisim', path)) stop("Un dossier 'multisim' doit etre present dans le repertoire de travail")
  
  for (date in dates) {
    cat(paste("\nImport des sorties de", date), "\n")
    if (grep(date, dates) == 1) {
      vgl <- data.frame()
      compar_vgl <- data.frame()
      
      if (length(dates) > 1 & dir_multisim) {
        multidate_folder <- file.path(path, "Resultats", paste(Sys.Date(), "Multiresults new"))
        dir.create(multidate_folder, recursive = T, showWarnings = F)
        vgl_rdata <- list.files(multidate_folder, "RData", full.names = T)
        cat(paste("Repertoire de travail:", multidate_folder, "\n"))
      }
    }
    vgl_folder <- act::find_dir(path, pattern = date)
    print(vgl_folder)
    if (length(vgl_folder) > 1) stop(paste0("Plusieurs dossier '", date, "'"))
    dir.create(vgl_folder, recursive = T, showWarnings = F)
    
    # Sauvegarde des fichiers de sortie ####
    zip_name <- paste0("Backup_Data_", date)
    zip_backup <- list.files(vgl_folder, zip_name, full.names = T)
    if (length(zip_backup) == 0) zip_backup <- list.files(vgl_folder, "Backup_Data.*zip", full.names = T)
    if (length(zip_backup) == 0) {
      cat(paste0("Pas d'archive 'Backup_Data' trouvee a ", vgl_folder, ".\nVeuillez indiquer le repertoire des fichiers de sorties pour la creation de l'archive."))
      output_path <- choose.dir(default = path)
      
      #liste usm
      if (length(list.files(path, "liste_usms_mix.csv")) == 0) {
        # L'import des fichiers xls a partir d'archive fonctionne mal
        df_usm <- readxl::read_xls(list.files(path, "liste_usms_mix.xls", full.names = T), sheet = "SimTest")
        write.csv2(data.frame(df_usm), file.path(output_path, "liste_usms_mix.csv"), row.names = F)
      }
      act::make_zip(vgl_folder, zip_name, "liste_usms_mix.csv", output_path)
      
      #output modele
      zip_files <- c("lsAxes", "toto", "scene", "Compare_Voxel_CaribuOrgane", "organ_voxel_position")
      act::make_zip(vgl_folder, zip_name, zip_files, output_path)
      
      #script
      act::make_zip(vgl_folder, zip_name, "l-egume_batch_mixture8", path)  # sauvegerde le script batch
      act::make_zip(vgl_folder, zip_name, "l-egume-1.0.37hquad_mixtures_initc.lpy", dirname(path))  # sauvegarde le script l-egume
      zip_backup <- list.files(vgl_folder, zip_name, full.names = T)
    } else cat(" Import de l'archive\n")
    listzip <- unzip(zip_backup, list = T)[, 1]
    
    # Import des donnees lsAxes ####
    list_lsaxes <- listzip[grep("lsAxes", listzip)]
    df_vgl <- act::legume_import_lsAxes(lsAxes_list = list_lsaxes, zip_path = zip_backup, organs = c("Pet", "In"))
    
    # Ajout des calculs de zeta ####
    zeta_file <- listzip[grep("Compare_Voxel_CaribuOrgane.dat", listzip)]
    zeta <- act::legume_import_compare(zeta_file, zip_backup)
    join_by <- colnames(zeta)[colnames(zeta) %in% colnames(df_vgl)] # doit etre: c("DOY", "ID_usm", "organ", "age", "nump", "nsh", "rank", "ordre", "modele_lum")
    cat(" zeta join")
    df_vgl <- plyr::join(df_vgl, zeta, by = join_by)
    cat(" OK\n")
    
    # Distance entre organe et face du voxel ####
    if (pos_org_vox & length(grep("organ_voxel_position", listzip)) > 0) {
      cat(" dist_vox join")
      listfiles <- listzip[grep("organ_voxel_position", listzip)]
      dist_vox <- data.frame()
      for (file in listfiles) {
        df <- read.csv(unz(zip_backup, file), sep = ";")
        colnames(df) <- c("DOY", "ID_usm", "organ", "age", "nump", "nsh", "rank", "ordre",
                          "modele_lum", "taille_voxel", "organ_id",
                          "organ_xyz", "sensor_xyz", "z_distance")
        dist_vox <- rbind(dist_vox, df)
      }
      
      dist_vox <- dist_vox[, grep("organ_xyz|sensor_xyz", colnames(dist_vox), invert = T)]
      try(dist_vox[dist_vox$modele_lum == 1, ]$modele_lum <- "RIRI", silent = T)  #"Voxel"
      try(dist_vox[dist_vox$modele_lum == 2, ]$modele_lum <- "CANESTRA Face", silent = T)  # "Face"
      try(dist_vox[dist_vox$modele_lum == 3, ]$modele_lum <- "CANESTRA Organ", silent = T)  # "Organ"
      
      colnames(dist_vox) <- tolower(colnames(dist_vox))
      join_by <- colnames(dist_vox)[colnames(dist_vox) %in% colnames(df_vgl)]
      df_vgl <- plyr::join(df_vgl, dist_vox, by = join_by)
      rm(listfiles, dist_vox, file, df)
      cat(" OK\n")
    }
    
    # Import sorties toto ####
    toto_list <- listzip[grep("toto", listzip)]
    toto <- act::legume_import_toto(toto_list, zip_backup)
    df_vgl <- plyr::join(df_vgl, toto, by = c("ID_usm", "nump", "DOY"))
    cat(" toto join OK\n")
    
    # Mise en forme facteurs ####
    df_vgl$organ <- as.character(df_vgl$organ)
    df_vgl[df_vgl$organ == "In", "organ"] <- "Internode"
    df_vgl[df_vgl$organ == "Pet", "organ"] <- "Petiole"
    df_vgl$organ <- as.factor(df_vgl$organ)
    
    df_vgl$densite <- as.numeric(as.character(df_vgl$densite))
    df_vgl$densite <- as.factor(paste0(df_vgl$densite, "plt.m-2"))
    df_vgl$densite <- relevel(df_vgl$densite, "11plt.m-2")
    
    df_vgl$modele <- as.character(df_vgl$modele)
    df_vgl[grep("riri", df_vgl$modele), "modele"] <- "riri"
    df_vgl[grep("caribu", df_vgl$modele), "modele"] <- "caribu"
    df_vgl$modele <- as.factor(df_vgl$modele)
    
    df_vgl$variete <- as.character(df_vgl$variete)
    df_vgl[grep("giga", df_vgl$variete), "variete"] <- "white clover" #"SF"
    df_vgl[grep("Fix2", df_vgl$variete), "variete"] <- "alfalfa" #"G-"
    df_vgl$variete <- as.factor(df_vgl$variete)
    
    df_vgl$modele_lum <- as.character(df_vgl$modele_lum)
    df_vgl$modele_lum <- as.factor(df_vgl$modele_lum)
    
    df_vgl$redif <- as.character(df_vgl$redif)
    df_vgl$redif <- as.factor(sort(df_vgl$redif))
    df_vgl$condition <- paste(df_vgl$modele_lum, df_vgl$redif)
    
    # Modification des donnees ####
    cat(" Modification des donnees")
    df_vgl$date_simul <- date
    df_vgl <- df_vgl[df_vgl$condition != "RIRI redif",]  # conserve les simuls de RIRI sans rediffusion
    df_vgl$Long <- df_vgl$Long * 100  # converti les m en mm
    df_vgl$nump <- df_vgl$nump + 1  # Permet de ne pas avoir de 0 qui genent certaines fonctions (couleurs des graphs)
    df_vgl$rank <- df_vgl$rank + 1
    df_vgl$relative_length <- df_vgl$l / df_vgl$Lmax
    df_vgl <- df_vgl[df_vgl$rank %in% unique(df_vgl$rank), ]  #selection de rangs pour les analyses
    df_vgl <- df_vgl[df_vgl$ordre == 1, ]  # Selection des ordres
    # df_vgl <- df_vgl[df_vgl$nsh != 0, ]  # Nombre de tiges
    df_vgl <- rbind(
      df_vgl[df_vgl$variete == "white clover" & df_vgl$organ == "Internode" & df_vgl$nsh == 1,],
      df_vgl[df_vgl$variete == "white clover" & df_vgl$organ == "Petiole" & df_vgl$nsh == 0,],
      df_vgl[df_vgl$variete == "alfalfa" & df_vgl$nsh == 0,]
    )
    if (!"taille_voxel" %in% colnames(df_vgl)) df_vgl$taille_voxel <- 2  # rajoute la taille des voxels par defaut si pas dans les donnees
    cat(" OK\n")
    
    # Transformation du tableau de donnees pour que chaque modele ait le meme nombre de lignes ####
    if (compar) {
      cat(" Comparaison des modeles")
      vgl_model <- data.frame(df_vgl[0, c("ordre", "organ", "densite", "nump", "nsh", "rank", "statut", "doy", "variete", "redif", "taille_voxel")])
      join_by <- colnames(vgl_model)
      for (modele_lum in unique(df_vgl$modele_lum)) {
        df <- na.omit(df_vgl[df_vgl$modele_lum == modele_lum, c(join_by, "age", "Zeta_RF", "TrPAR", "l", "Long")])
        df$modele_lum <- NULL
        df$TT <- NULL
        colnames(df)[grep("age|Zeta_RF|TrPAR|^l$|Long", colnames(df))] <- paste0(colnames(df)[grep("age|Zeta_RF|TrPAR|^l$|Long", colnames(df))], "_", modele_lum)
        vgl_model <- plyr::join(vgl_model, df, by = join_by, type = "full")
      }
      compar_vgl <- rbind(compar_vgl, vgl_model)
      rm(modele_lum, df)
      cat(" OK\n")
    }
    
    # Fusion des donnees ####
    vgl <- rbind(vgl, df_vgl)
    rm(list_lsaxes, zeta_file, zeta, join_by, toto_list, toto, df_vgl)
  }
  
  # Filtre des rangs arrive a maturite ####
  if (mature) {
    conditions <- act::unique_table(vgl[vgl$statut == "mat",], c("variete", "organ", "rank"))
    filtre <- data.frame()
    for (i in seq_len(nrow(conditions))) {
      df <- act::act_call(vgl, conditions, i)
      filtre <- rbind(filtre, df)
      act::progress_bar(i, nrow(conditions), "filtre rangs")
    }
    vgl <- filtre
    vgl <- act::order_column(vgl, c("variete", "organ", "densite"))
    rm(conditions, filtre, i, df)
  }
  
  #Fin Import ####
  opt <- list(ann = TRUE, pal = 2, res = 600, png_output = FALSE, plot_size = 4)
  if (length(dates) > 1 & dir_multisim) {
    vgl_folder <- multidate_folder
    rm(multidate_folder)
  }
  
  colnames(vgl)[grep("echelle", colnames(vgl))] <- "modeling_method"
  colnames(vgl)[grep("densite", colnames(vgl))] <- "density"
  colnames(vgl)[grep("Long", colnames(vgl))] <- "length"
  colnames(vgl)[grep("variete", colnames(vgl))] <- "morphogenetic_group"
  colnames(vgl)[grep("redif", colnames(vgl))] <- "scattering"
  vgl$zeta <- as.numeric(vgl$Zeta_RF)
  
  return(vgl)
  rm(path, date, zip_backup, listzip, t, dates)
}
