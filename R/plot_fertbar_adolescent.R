#' Creates adolescent fertility rate visualization
#'
#' @description This function plots a bar graph showing adolescent fertility rate.
#'
#' @author Britta Schumacher
#'
#' @import ggplot2
#' @importFrom dplyr filter arrange
#' @importFrom scales breaks_width
#' @importFrom finalfit round_tidy
#'
#' @param fert_data Country mortality data in long format.
#'
#' @examples
#' plot_mr(fert)
#'
#' @returns A ggplot bar graph with total births per adolscent (ages 15-19)
#' woman on the x axis and district on the y-axis..
#'
#' @export
#'
#'
# Adolescent Fertility Rate in 2018
plot_fertbar_adolescent <- function(fert_data,adm_level) {

  p <- fert_data |>
    dplyr::filter(ADM_level == adm_level,
                  YR == 2018,
                  metric %in% c("adolFR")) |>
    dplyr::arrange(metric) |>
    ggplot(aes(x = reorder(area_name, value), y = value)) +
    geom_text(aes(label = finalfit::round_tidy(value, digits = 2)), hjust = -0.3, size = 2.7) +
    scale_y_continuous(breaks = scales::breaks_width(0.25)) +
    geom_bar(fill = "#78c679", stat = "identity", alpha = 0.8, show.legend = FALSE, width = 0.8) +
    ggtitle(paste0("Adolescent Fertility Rate by District, Malawi 2018")) +
    labs(y = "Total Births per Woman Ages 15-19",
         x = NULL) +
    coord_flip() +
    theme_plot()

  return(p)
}
