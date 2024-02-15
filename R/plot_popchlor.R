#' Title
#'
#' @param adm_sf
#' @param pop_data
#' @param country
#' @param year
#' @param breaks
#' @param adm_level
#'
#' @param
#'
#' @import ggplot2
#' @import viridis
#' @import scales
#' @importFrom stringr str_to_title
#'
#' @return
#' @export
#'
#' @examples
plot_chlor <- function(adm_sf,
                       pop_data,
                       country,
                       yr,
                       adm_level = "ADM1",
                       breaks = NULL) {

  if (paste0("ADM", unique(adm_sf[["ADM_LEVEL"]])) != adm_level) {
    stop("`adm_level` does not match ADM level of `adm_sf`")
  }


  message("- Step 1 of 3: Joining data")
  message("- Step 2 of 3: Simplifying data")
  joined_df <- join_data_to_shp(pop_data, adm_sf, adm_level = adm_level, yr = yr)

  message("- Step 3 of 3: Plotting chloropleth...please be patient")
  ggplot() +
    geom_sf(data = joined_df,
            aes(fill = POP_TOTAL),
            size = 0,
            alpha = 0.9) +
    viridis::scale_fill_viridis(trans = "log",
                                breaks = scales::breaks_log(n = 5, base = 10),
                                labels = scales::comma,
                                name = "Population",
                                guide = guide_legend(keyheight = unit(3, units = "mm"),
                                                     keywidth=unit(12, units = "mm"),
                                                     label.position = "right",
                                                     title.position = "top")) +
    labs(
      title = paste0("Population across ", stringr::str_to_title(country), " in ", yr),
      #subtitle = "XXX",
      caption = "Source: U.S. Census Bureau | International Programs Center | International Database"
    ) +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),

      plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
      plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),

      legend.position = c(0.7, 0.09)
    ) +
    theme_void()
}
