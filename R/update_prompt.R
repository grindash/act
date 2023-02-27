#' Add time to console prompt
#'
#' @export
prompt_with_date <- function() {
  options(prompt = paste(Sys.time(),"> "))
}