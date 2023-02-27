#' Create and update zip file
#'
#' @param zip_path path of the zip archive
#' @param zip_name name of the archive
#' @param files_to_zip filenames of files to add in the archive
#' @param file_path path to file to archive. default is identical as zip_path.
#' @param software_path path to zip software
#' @param ... argument to pass to zip()
#'
#' @return zip file
#' @export
make_zip <- function(zip_path, zip_name=NULL, files_to_zip, file_path=NULL, software_path = "C:\\Program Files\\7-Zip\\7Z", ...) {
  if (is.null(zip_name)) zip_name <- basename(zip_path)
  if (is.null(file_path)) file_path <- zip_path
  zip_name <- gsub(".zip", "", zip_name)
  
  zip_backup <- list.files(zip_path, zip_name, full.names = T)
  if (length(zip_backup) > 1) stop("Plusieurs archives avec le meme nom")
  flags <- " a -tzip"
  
  if (is.null(zip_backup)) {  # verifie si le zip existe deja
    for (output in files_to_zip) {
      files <- list.files(file_path, output, recursive = F,  full.names = F)
      for (file in files) {
        act::progress_bar(grep(file, files) - 1, length(files), paste("zip creation |", output))
        zip(zipfile = file.path(zip_path, zip_name),
            files = file.path(file_path, file),
            flags = flags,  # add to a zip archive
            extras = "",
            zip = software_path, # use 7z program" https://stackoverflow.com/a/47371159
            ...)  
        act::progress_bar(grep(file, files), length(files), paste("zip creation |", output))
      }
    }
    zip_backup <- list.files(zip_path, paste0(zip_name, ".zip"), full.names = T)
    
  } else {
    for (output in files_to_zip) {
      files <- list.files(file_path, output, recursive = F, full.names = F)
      for (file in files) {
        act::progress_bar(grep(file, files) - 1, length(files), paste("zip update |", output))
        if (length(grep(file, unzip(zip_backup, list = T)[, "Name"])) == 0) {  # verifie si le fichier est deja dans le zip
          zip(zipfile = file.path(zip_path, zip_name),
              files = file.path(file_path, file),
              flags = flags,
              extras = "",
              zip = software_path,
              ...)
          
          #Verifie si l'archivage a fonctionne
          if (length(grep(file, unzip(zip_backup, list = T)[, "Name"])) == 0) {
              file.copy(file.path(file_path, file), getwd())
              zip(zipfile = file.path(zip_path, zip_name),
                  files = file.path(getwd(), file),
                  flags = flags,
                  extras = "",
                  zip = software_path,
                  ...)
              if (file_path != getwd()) file.remove(file.path(getwd(), file))  # empeche de supprimer le fichier d'origine
          }
        }
        act::progress_bar(grep(file, files), length(files), paste("zip update |", output))
      }
    }
  }
  # system(paste('"C:/Program Files/7-Zip/7z.exe"', "a -tzip", shQuote(file.path(zip_path, zip_name)), shQuote(file), '> NUL'))
}
