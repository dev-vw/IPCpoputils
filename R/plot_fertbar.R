#' Creates total fertility rate visualization
#'
#' @description This function plots a bar graph showing total fertility rate.
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
#' @returns A ggplot bar graph with total births per woman on the x axis and
#' district on the y-axis..
#'
#' @export
#'
#'
# Total Fertility Rate (all women over 15) by District in 2018
plot_fertbar <- function(fert_data, adm_level) {

  p <- fert_data |>
    dplyr::filter(ADM_level == adm_level,
                  YR == 2018,
                  metric %in% c("TFR")) |>
    dplyr::arrange(metric) |>
    ggplot(aes(x = reorder(area_name, value), y = value)) +
    geom_bar(fill = '#41AB5D',
             stat = "identity",
             alpha = 0.8,
             show.legend = FALSE,
             width = 0.8) +
    geom_text(aes(label = scales::label_number(accuracy = 0.01)(value)),
              hjust = -0.3,
              size = 3,
              color = "black") +  # Add labels with consistent formatting
    scale_y_continuous(breaks = scales::breaks_width(1)) +
    ggtitle("Total Fertility Rate by District, Malawi 2018") +
    labs(y = "Total Births per Woman",
         x = NULL) +
    coord_flip() +
    theme_plot()

  return(p)
}

