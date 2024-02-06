library(tidyverse)
library(ggplot2)
library(lemon)
library(gridExtra)

#' required data format ---
#' selected year: e.g. 2015
#' adm selection: e.g. 1
#' sex: only M/F
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
    scale_x_symmetric(labels = abs,
                      breaks = pop_range_seq) +
    ggtitle(toupper(adm_name)) +
    theme(legend.position = "none") +
    xlab("Population (in thousands)") +
    ylab(paste0("Age, ", tolower(age_group), "-year age groups")) +
    labs(subtitle = "Source: U.S. Census Bureau | International Programs Center | International Database")

  return(p)
}

#'
plot_pyr_subnat <- function(country_data, age_group, adm_var) {
  adm_names <- unique(country_data[[adm_var]])
  print(adm_names)

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

