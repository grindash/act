#' Return a part of a data frame corresponding to a condition
#'
#' @param data data frame
#' @param unique_table List of unique conditions, can be created with AOB::unique_table.
#' @param scenario_nb Row of unique_table to compute, compute every row if not specified. Useful in loop.
#' @param rt return format
#'
#' @return Data frame with asked condition.
#' @export
#'
#' @examples data("CO2")
#' @examples ut <- unique(CO2[, 1:3])
#' @examples uptake_mean <- data.frame()
#' @examples for (i in 1:nrow(ut)) {uptake_mean <- rbind(uptake_mean, Rmisc::group.CI(uptake ~ Plant, act_call(CO2, ut, i)))}
act_call <- function(data, unique_table, scenario_nb, rt="df") {
  if (is.null(colnames(unique_table))) stop("no column in unique_table, you should use df[, col, drop = F]")
  call <- NULL
  for (col in colnames(unique_table)) call <- paste(call, data[, col] %in% unique_table[scenario_nb, col], sep = " & ")
  # call <- sapply(colnames(unique_table), function(x) {paste(data[, x]%in%unique_table[scenario_nb, x], sep=" & ")})
  # call <- apply(call[, colnames(call)], 1, paste, collapse = " & " )

  call[grep("FALSE", call)] <- FALSE  # Transforme l'emsemble des arguments en FALSE si il y en a au moins 1 dans la liste
  call[grep("TRUE", call)] <- TRUE  # Transforme les listes restantes qui ne contenaient pas de FALSE en TRUE
  call <- as.logical(call)

  if (rt == "call") return(call)
  if (rt == "df") return(data[call, ])
}
