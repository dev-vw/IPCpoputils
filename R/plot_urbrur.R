#' Creates urban / rural population visualization
#'
#' @description This function plots a bar graph showing the urban / rural population breakdown in a given geography.
#'
#' @author Britta Schumacher
#'
#' @importFrom ggplot2
#' @importFrom dplyr filter mutate
#' @importFrom scales label_number
#'
#' @param pop_data Country demographic data in long format.
#'
#' @examples
#' plot_urban_rural(pop,"Malawi")
#'
#' @returns A ggplot bar graph with total population on the x axis and 
#' urban / rural on the y-axis..
#'
#' @export
#' 
#' 
#   Population by Urban / Rural Residence
plot_urbrur <- function(pop_data, adm_name) {
  
  p <- pop_data %>%
  dplyr::filter(metric %in% c("total_rural","total_urban") 
                & area_name == adm_name) %>% # geography dropdown
  dplyr::mutate(UrbanRural = forcats::fct_relevel(metric, "total_urban", "total_rural")) %>%
  ggplot(aes(x = metric, y = value/1000, fill = metric)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  scale_fill_manual(values = c("#31a354","#006837")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  ggtitle(paste0("Population by Urban / Rural Residence, ",adm_name," 2018")) +
  ylab(("People (in thousands)")) +
  xlab("") +
  scale_x_discrete(labels = c("Rural","Urban")) +
  coord_flip() +
  theme_plot(aspect.ratio = 0.4)

p
}