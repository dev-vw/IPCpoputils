#' Creates population by District visualization
#'
#' @description This function plots a bar graph showing the population breakdown
#' by district organized from greatest to least population in 2018.
#'
#' @author Britta Schumacher
#'
#' @importFrom ggplot2
#' @importFrom dplyr filter arrange
#' @importFrom scales breaks_width
#' @importFrom gghighlight gghighlight
#'
#' @param pop_data Country demographic data in long format.
#'
#' @examples
#' plot_pop(pop,"Balaka")
#'
#' @returns A ggplot bar graph with total population on the x axis and
#' district on the y-axis. Plot highlights specific geography when "Malawi"
#' is not selected in the geography dropdown.
#'
#' @export
#'
#'
#
# Total Population by District
plot_pop <- function(pop_data, adm_name) {

  p <- pop |>
    dplyr::filter(ADM_level == "ADM2",
                  metric %in% c("POP_2018")) |>
    dplyr::arrange(metric) |>
    ggplot(aes(x = reorder(area_name, value), y = value/1000)) +
    #geom_text(aes(label = value), hjust = -0.3, size = 2.7) +
    scale_y_continuous(breaks = scales::breaks_width(500)) +
    geom_bar(fill = '#ffffcc', stat = "identity", alpha = 0.8, show.legend = FALSE, width = 0.8) +
    ggtitle(paste0("Population by District, Malawi 2018")) +
    ylab(("People (in thousands)")) +
    xlab(("")) +
    coord_flip() +
    theme_plot()

  if(adm_name != "Malawi") {
    p <- p  + gghighlight::gghighlight(area_name == adm_name) # geography dropdown
    p
  } else {
    p
  }

}
