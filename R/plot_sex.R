#' Creates population by sex visualization
#'
#' @description This function plots a bar graph showing the population breakdown by sex in a given geography.
#'
#' @author Britta Schumacher
#'
#' @importFrom ggplot2
#' @importFrom dplyr filter mutate
#' @importFrom scales label_number
#' @importFrom finalfit round_tidy
#' @importFrom forcats fct_relevel
#'
#' @param pop_data Country demographic data in long format.
#'
#' @examples
#' plot_sex(pop,"Malawi")
#'
#' @returns A ggplot bar graph with total population on the x axis and
#' male / female on the y-axis..
#'
#' @export
#'
#'
# Population by Sex
plot_sex <- function(pop_data, adm_name) {
  #
  p <- pop_data %>%
    dplyr::filter(metric %in% c("POPm_2018","POPf_2018")
                  & area_name == adm_name) |> # geography dropdown
    dplyr::mutate(Sex = forcats::fct_relevel(metric, "POPm_2018","POPf_2018")) %>%
    ggplot(aes(x = Sex, y = value/1000, fill = Sex)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("#addd8e","#31a354")) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    scale_x_discrete(labels = c("POPm_2018" = "Male", "POPf_2018" = "Female")) +
    ggtitle(paste0("Population by Sex, ",adm_name," 2018")) +
    ylab(("People (in thousands)")) +
    xlab("") +
    theme_plot(aspect.ratio = 0.4) +
    coord_flip()

  p
}
