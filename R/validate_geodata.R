#' @param match_name
#'
#' @param adm_sf
#'
#' @importFrom sf st_is_valid
validate_geodata <- function(match_name, adm_sf) {
  if (!all(st_is_valid(adm_sf))) {
    stop("The input shapefile data frame is corrupted. \nPlease consider fixing before proceeding.")
  }

  if (!(tolower(match_name) %in% tolower(colnames(adm_sf)))) {
    stop("`match_name` does not exist in the shapefile data frame.")
  }

  if (any(duplicated(adm_sf[[match_name]]))) {
    stop("`match_name` is not unique.")
  }

  return(TRUE)
}
