#' Creates life expectancy visualization
#'
#' @description This function plots a bar graph showing life expectancy at birth 
#' on the x axis and sex (M/F)on the y axis.
#'
#' @author Britta Schumacher
#'
#' @importFrom ggplot2
#' @importFrom dplyr filter mutate arrange
#' @importFrom forcats fct_relevel
#' @importFrom scales label_number
#'
#' @param mort_data Country mortality data in long format.
#' @param adm_name Name of administrative geography (i.e., nation or district).
#'
#' @examples
#' plot_le(mort, "Malawi")
#'
#' @returns A ggplot bar graph with life expectancy at birth on the x axis and sex (M/F)
#' on the y axis.
#'
#' @export
#' 
#' 
# Life Expectancy by geography and sex in 2018
plot_lebar <- function(mort_data, adm_name) {
  
  p <- mort_data |> 
    dplyr::filter(area_name == adm_name, # geography dropdown
                  YR == 2018,
                  metric %in% c("male_le", "female_le")) |>
    dplyr::mutate(metric = forcats::fct_relevel(metric, "male_le", "female_le")) |> 
    dplyr::arrange(metric) |> 
    ggplot(aes(x = metric, y = value, fill = metric)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("#FFFFB2", "#FD8D3C")) +
    ggtitle(paste0("Life Expectancy at Birth, ",adm_name," 2018")) +
    scale_x_discrete(
      labels = c("male_le" = "Male",
                 "female_le" = "Female")) +
    ylab(("Years")) +
    xlab(("")) +
    coord_flip() +
    theme_plot()
  
  return(p)
}
