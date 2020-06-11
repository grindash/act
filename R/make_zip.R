#' Create and update zip file
#'
#' @param zip_path path to zip archive
#' @param zip_name name of the archive. Do not add ".zip" at the end
#' @param zip_files filenames of files to add in the archive
#' @param file_path path to file to archive. default is identical as zip_path.
#'
#' @return
#' @export
make_zip <- function(zip_path, zip_name="Backup_Data", zip_files, file_path=NULL) {
  if (length(file_path) == 0) file_path <- zip_path
  zip_backup <- list.files(zip_path, paste0(zip_name, ".zip"), full.names = T)
  if (length(zip_backup) == 0) {
    for (output in zip_files) {
      files <- list.files(file_path, output, recursive = T,  full.names = T)
      zip(zipfile = file.path(zip_path, zip_name),
          files = files,
          flags = " a -tzip",  # add to a zip archive
          zip = "C:\\Program Files\\7-Zip\\7Z")  # use 7z program" https://stackoverflow.com/a/47371159
    }
    zip_backup <- list.files(zip_path, paste0(zip_name, ".zip"), full.names = T)
  } else {
    for (output in zip_files) {
      if (length(grep(output, unzip(zip_backup, list = T)[, "Name"])) == 0) {
        files <- list.files(file_path, output, recursive = T,  full.names = T)
        zip(zipfile = file.path(zip_path, zip_name),
            files = files,
            flags = " a -tzip",
            zip = "C:\\Program Files\\7-Zip\\7Z")
      }
    }
  }
}
