#' Title
#'
#' @param pop_data pop df
#' @param adm_name a string
#'
#' @return a plot
#' @export
plot_popts <- function(pop_data, adm_name) {
  p <- ggplot(data = pop_data,
              mapping = aes(x = YR,
                            y = POP_TOTAL/1000,
                            color = SEX)) +
    geom_point(aes(fill = SEX), shape = 21, color = "white", size = 0.5) +
    geom_line(aes(color = SEX)) +
    scale_x_continuous(labels = as.character(pop_data$YR), breaks = pop_data$YR) +
    ggtitle(toupper(adm_name)) +
    theme(legend.position = "none") +
    ylab("Population (in thousands)") +
    xlab("Year") +
    labs(subtitle = "Source: U.S. Census Bureau | International Programs Center | International Database")

  return(p)
}

#' add a rm.na var
plot_popts_subnat <- function(country_data, adm_var) {
  adm_names <- unique(country_data[[adm_var]])
  print(adm_names)

  plots <- lapply(adm_names, function(name) {
    df <- country_data[country_data[[adm_var]] == name, ]
    p <- plot_popts(df, name)
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
