#'
pretty_symmetric <- function(vec) {
  rvec <- pretty(range(vec))
  lvec <- sort(-rvec)[1:length(rvec)-1]
  return(c(lvec, rvec))
}

#'
pretty_agecats <- function(agecat_vec) {
  vec <- gsub("(?<=\\d)_(?=\\d)", "-", agecat_vec, perl = TRUE) # for 0-4, 5-9
  vec <- gsub("(?<=\\d)_$", "+", vec, perl = TRUE) # for 100+

  return(vec)
}
