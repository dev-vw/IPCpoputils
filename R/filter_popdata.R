#' filters an already clean pop dataset, according to params
#'
#' @param pop_data needs to be an already cleaned pop_data
#' @param yr a vec of values
#' @param adm_level an int
#' @param sex a vec of values
#'
#' @importFrom dplyr filter
#' @export
filter_popdata <- function(pop_data, yr = NULL, adm_level = 0, sex = c("B")) {
  if (any(!sex %in% c("M", "F", "B"))) {
    message("`sex` input is invalid. Accepted values are `M`, `F`, and `B`")
  }

  if (!adm_level %in% unique(pop_data$ADM_LEVEL)) {
    message("`adm_level` is out of bounds")
  }

  if (is.null(yr)) {
    yr <- c(min(pop_data$YR):max(pop_data$YR))
  } else {
    if (any(!yr %in% unique(pop_data$YR))) {
      message("`yr` is out of bounds")
    }
  }

  df <- pop_data %>% filter(ADM_LEVEL == adm_level,
                         YR %in% yr,
                         SEX %in% sex)

  return(df)
}

