#' Make scale symmetric and pretty
#'
#' @description Developed specifically for plotting population pyramids. Given
#' a ascending sequence of numbers, creates a symmetric scale mirrored across
#' the number line, with 0 as the midpoint.
#'
#' @param vec An ascending sequence of numbers greater than 0.
#'
#' @returns A pretty numeric vector
pretty_symmetric <- function(vec) {
  if (any(vec <= 0)) {
    stop("`vec` cannot contain 0 or negative numbers.")
  }

  rvec <- pretty(range(vec))
  lvec <- sort(-rvec)[1:length(rvec) - 1]

  return(c(lvec, rvec))
}

#' Make five-year age categories pretty.
#'
#' @description pop_data five-year age categories are formatted as "0_4",
#' "5_9", ... , "75_79", "80_". This function reformats these categories as
#' "0-4", "5-9", ... , "75-79", "80+".
#'
#' @param agecat_vec A vector of strings specifying age categories.
#'
#' @returns A pretty vector of strings
pretty_agecats <- function(agecat_vec) {
  if (!all(grepl("(?<=\\d)_(?=\\d)|(?<=\\d)_$", agecat_vec, perl = TRUE))) {
    stop("`agecat_vec` is not in the right format. All numbers must be NUM<underscore>NUM or NUM<underscore>.")
  }

  vec <- gsub("(?<=\\d)_(?=\\d)", "-", agecat_vec, perl = TRUE) # for 0-4, 5-9
  vec <- gsub("(?<=\\d)_$", "+", vec, perl = TRUE) # for 100+

  return(vec)
}
