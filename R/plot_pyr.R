#' Plot a population pyramid
#'
#' @description
#' A short description...
#'
#' `plot_pyr` plots a single population pyramid. The input data is `pop_data` and
#' has strict requirements for the pyramid to render correctly:
#'
#' * The YR column should have one year only
#' * The ADM_LEVEL column should have one level only
#' * The SEX column should have two values only
#'
#' @param pop_data A data frame containing population data.
#' @param adm_name A string.
#' @param age_group A choice between `one` or `five`.
#'
#' @import ggplot2
#' @importFrom lemon scale_x_symmetric
#'
#' @returns a plot
#'
#' @example
#' @export
plot_pyr <- function(pop_data, adm_name, age_group) {

  pop_range_seq <- pretty_symmetric(pop_data$POP_TOTAL/1000)
  levels(pop_data$AGE_CAT) <- pretty_agecats(levels(pop_data$AGE_CAT))

  p <- ggplot(data = pop_data,
              mapping = aes(x = ifelse(test = SEX == "M",
                                       yes = -POP_TOTAL/1000,
                                       no = POP_TOTAL/1000),
                            y = as.factor(AGE_CAT),
                            fill = SEX)) +
    geom_col() +
    lemon::scale_x_symmetric(labels = abs,
                             breaks = pop_range_seq) +
    ggtitle(toupper(adm_name)) +
    theme(legend.position = "none") +
    xlab("Population (in thousands)") +
    ylab(paste0("Age, ", tolower(age_group), "-year age groups")) +
    labs(subtitle = "Source: U.S. Census Bureau | International Programs Center | International Database")

  return(p)
}

#' Plot subnational population pyramids
#'
#' `plot_pyr_subnat` generates a list population pyramid plots. The input data is
#' `pop_data` and has strict requirements for the pyramid to render correctly:
#'
#' * The YR column should have one year only
#' * The SEX column should have two values only
#'
#' @param country_data A data frame containing population data.
#' @param age_group A choice between `one` or `five`.
#' @param adm_var The column containing the ADM level of each entry. Defaults to `ADM_LEVEL`.
#'
#' @returns a list of plots
#'
#' @examples
#'
#'
#' @export
plot_pyr_subnat <- function(country_data, age_group, adm_var = "ADM_LEVEL") {
  adm_names <- unique(country_data[[adm_var]])

  plots <- lapply(adm_names, function(name) {
    df <- country_data[country_data[[adm_var]] == name, ]
    p <- plot_pyr(df, name, age_group)
  })

  return(plots)

  # print('Saving plots...')
  #
  # ggsave(
  #   filename = paste0("deliverables/subnational/", country, "_adm1.pdf"),
  #   plot = marrangeGrob(plots, nrow = 2, ncol = 1, top = NULL),
  #   width = 8.5,
  #   height = 11,
  #   units = "in"
  # )
}

#' TODO: create a save plots function

