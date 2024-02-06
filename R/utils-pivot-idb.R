#' @description Function to create list of data frames, each containing area
#' names and GEO_MATCH codes.
#'
#' @author Vania Wang
#'
#' @importFrom dplyr group_by summarize
#'
#' @param n an integer
#'
#' @returns A data frame.
#'
#' @export
adm_name_gen <- function(n) {
  print(paste0('Processing ADM', n, ' names'))
  df <- dat[dat$ADM_LEVEL == n, ] %>%
    dplyr::group_by(GEO_MATCH, AREA_NAME, ADM_LEVEL) %>%
    dplyr::summarize()

  return(df)
}

#' @description Correctly subsets GEO_MATCH string.
#'
#' @author Vania Wang
#'
#' @importFrom stringr str_split
#'
#' @param vec
#' @param i
#'
#' @returns A data frame.
#'
#' @export
generate_geomatch_vecs <- function(vec, i) {
  vec_split <- stringr::str_split(dat$GEO_MATCH, '_')
  vec_sub <- lapply(vec_split, function(vec) vec[1:i])
  vec_cat <- unlist(lapply(vec_sub, function(vec) paste(vec, collapse = '_')))

  return(vec_cat)
}
