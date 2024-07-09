#' Title
#'
#' @param pop_data
#' @param pop_sf
#' @param adm_level A numeric specifying ADM level
#' @param yr
#'
#' @return
#' @export
#'
#' @examples
join_data_to_shp <- function(pop_data, pop_sf, adm_level = "ADM1", yr = 2020) {

  # TODO
  # tryCatch(
  #   {
  #     validate(pop_data)
  #     validate(pop_sf)
  #   },
  #   error = function(cond) {
  #     stop()
  #   },
  #   warning = function(cond) {
  #     warning()
  #   }
  # )

  message("- Joining data")

  if (paste0("ADM", unique(pop_sf[["ADM_LEVEL"]])) != adm_level) {
    stop("`adm_level` does not match ADM level of `adm_sf`")
  }

  # does adm_level exist in pop_sf and pop_data? If yes, filter pop_df
  # for relevant adm_level.
  if ("ADM_LEVEL" %in% colnames(pop_data) & "ADM_LEVEL" %in% colnames(pop_sf)) {
    pop_data_filtered <- filter_popdata(pop_data,
                                        yr = yr,
                                        adm_level = substr(adm_level, nchar(adm_level), nchar(adm_level)))

    pop_data_filtered <- pop_data_filtered %>% group_by(GEO_MATCH, ADM4) %>% summarize(POP_TOTAL = sum(POP_TOTAL))

    joined_df <- left_join(pop_sf,
                           pop_data_filtered,
                           by = "GEO_MATCH")

    return(joined_df)
  } else {
    stop("Either `pop_data` or `pop_sf` does not have the required column, `ADM_LEVEL`.")
  }
}
