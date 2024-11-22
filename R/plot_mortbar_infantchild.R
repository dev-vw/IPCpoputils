#' Plots infant and child mortality bar graphs
#'
#' @description This function plots a bar graph showing infant and child mortality.
#'
#' @author Britta Schumacher
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate arrange
#' @importFrom forcats fct_relevel
#' @importFrom scales label_number
#'
#' @param mort_data Country mortality data in long format.
#' @param adm_name Name of administrative geography (i.e., nation or district).
#'
#' @examples
#' plot_mr(mort_data, "Malawi")
#'
#' @returns A ggplot bar graph with Deaths per 1,000 live births on the x axis and infant (< 1yr)
#' and child (< 5yrs) on the y axis.
#'
#' @export
#'
#'
# IMR/CMR by geography in 2018
plot_mortbar_infantchild <- function(mort_data, adm_name) {

  p <- mort_data |>
    dplyr::filter(area_name == adm_name, # geography dropdown
                  YR == 2018,
                  metric %in% c("IMR", "U5M")) |>
    dplyr::mutate(metric = forcats::fct_relevel(metric, "IMR", "U5M")) |>
    dplyr::arrange(metric) |>
    ggplot(aes(x = metric, y = value * 1000, fill = metric)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE, width = 0.8) +
    geom_text(aes(label = scales::label_number(accuracy = 1)(value * 1000)),  # Add labels
              hjust = -0.3,  # Adjust horizontal alignment
              size = 4) +  # Adjust size as needed
    scale_fill_manual(values = c("#feb24c", "#f03b20")) +
    ggtitle(paste0("Infant and Child Mortality, ", adm_name, " 2018")) +
    scale_x_discrete(
      labels = c("IMR" = "Infant Mortality",
                 "U5M" = "Child Mortality")) +
    ylab("Deaths per 1,000 live births") +
    xlab("") +
    coord_flip() +
    theme_plot()

  return(p)
}

