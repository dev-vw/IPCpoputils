#' cleans fertility data; not exported (for internal use only)
#'
#' @param raw_fert_path path to raw mortality data
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @import dplyr
#'
#' @examples
#' clean_fertdata("data-raw/mort-data/malawi.xlsx")
clean_fertdata <- function(raw_fert_path) {

  # data read in and clean up
  fert <- read_excel("data-raw/fert-data/fertility.xlsx",
                     sheet = 1)
  fert <- pivot_longer(fert,
                       3:5,
                       names_to = "metric",
                       values_to="value")
  fert <- fert |> dplyr::mutate(YR = substr(metric, nchar(metric) - 3, nchar(metric)),
                                metric = substr(metric, 1, nchar(metric) -5))

  return(fert)
}
