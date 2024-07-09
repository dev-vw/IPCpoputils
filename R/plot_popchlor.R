#' Plot a population choropleth map
#'
#' @description Given an ADM shapefile (adm_sf) and population data (pop_data),
#' returns a choropleth plot containing scaled population values. Defaults to
#' log transformed population.
#'
#'
#' @author Vania Wang
#'
#'
#' @param adm_sf A shapefile with `GEO_MATCH` or (equivalent) attribute.
#' @param pop_data A data frame containing population data
#' @param country A string
#' @param year A numeric specifying year
#' @param breaks A vector of breaks
#'
#' @import ggplot2
#' @import viridis
#' @import scales
#' @importFrom stringr str_to_title
#'
#' @returns A choropleth plot
#'
#' @export
plot_chlor <- function(joined_df,
                       country,
                       yr,
                       breaks = NULL) {

  message("- Plotting chloropleth...please be patient")
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
