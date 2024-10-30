#' cleans mortality data; not exported (for internal use only)
#'
#' @param raw_mort_path path to raw mortality data
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' clean_mortdata("./data-raw/mort-data/malawi.xlsx")
clean_mortdata <- function(raw_mort_path) {
  mort <- readxl::read_excel("./data-raw/mort-data/malawi.xlsx",
                             sheet = 1)
  mort <- tidyr::pivot_longer(mort,
                              3:10,
                              names_to = "metric",
                              values_to = "value") |>
    dplyr::mutate(YR = substr(metric,
                              nchar(metric)-3,
                              nchar(metric)),
                  metric = substr(metric,
                                  1,
                                  nchar(metric)-5))

  return(mort)
}


