#' Make all demographic data spatial
#'
#' Merges fertility and mortality data with country shapefiles at the
#' ADM2 level.
#'
#' @param demo_data df, must have a matching column named "area_name"
#' @param spatial_data sf, must have a matching column named "DIST_NAME"
#'
#' @importFrom dplyr filter left_join
#'
#' @return a df
#' @export
join_demodata_to_shp <- function(demo_data, spatial_data, geo_match) {

  message("- Joining data")

  demodata_filtered <- demo_data |>
    dplyr::filter(ADM_level == "ADM2")

  if (length(unique(demodata_filtered$area_name)) != length(unique(spatial_data$DIST_NAME))) {
    stop("Number of administrative units in 'demo_data' do not match number in `spatial_data`")
  }

  # does the number of unique administrative units match across demographic and spatial data?
  # If yes, join.
  if (length(unique(demodata_filtered$area_name)) == length(unique(spatial_data$DIST_NAME))) {

    joined_df <- dplyr::left_join(spatial_data,
                                  demodata_filtered,
                                  by = c("DIST_NAME" = "area_name"))

    return(joined_df)
  } else {
    stop("Inspect demographic and spatial data. Join not possible in current state.")
  }
}
