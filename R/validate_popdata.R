#' Validates the imported population data
#'
#' @param popdf
#'
#' @return
#' @export
#'
#' @examples
validate_popdata <- function(popdf) {

  # required columns that a popdf should definitely have
  req_cols <- c("adm_level", "geo_match", "adm0", "adm1",
                "yr", "sex", "age_cat", "pop_total")

  # check if all req_cols exists in popdf
  if (!all(req_cols %in% tolower(colnames(popdf)))) {
    warning("Required columns are not present in the input population data frame.\n")
  }

  # are there duplicate column names? throw a warning if yes
  if (any(duplicated(tolower(colnames(popdf))))) {
    warning("There are duplicated columns in the input population data frame. Only the first instance of a duplicated column will be used.\n")
  }

  # now examine the cols outside of req_cols and examine them
  # for validity
  diff_cols <- c(setdiff(tolower(colnames(popdf)), req_cols))

  valid_cols <- diff_cols[grepl("^adm\\d$", diff_cols)]
  invalid_cols <- diff_cols[!grepl("^adm\\d$", diff_cols)]

  # `diff_cols` of the format adm[NUM], are OK. Any other cols are
  # not needed. Throw a warning for these columns.
  if (length(valid_cols) != 0) {
    valid_col_str <- paste(valid_cols,
                           collapse = ", ")

    message(paste0("This application will use the following additional ADM levels: ",
                   valid_col_str,
                   "\n"))
  }

  if (length(invalid_cols) != 0) {
    invalid_col_str <- paste(invalid_cols,
                             collapse = ", ")

    message(paste0("The following columns will not be used by this application: ",
                   invalid_col_str,
                   "\n"))
  }


  # if (!all(grepl("adm\\d", diff_cols))) {
  #
  # }
}
