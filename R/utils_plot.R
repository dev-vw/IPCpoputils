#' Make scale symmetric and pretty
#'
#' @description Developed specifically for plotting population pyramids. Given
#' a ascending sequence of numbers, creates a symmetric scale mirrored across
#' the number line, with 0 as the midpoint.
#'
#' @param vec An ascending sequence of numbers greater than 0.
#'
#' @import ggplot2
#'
#' @returns A pretty numeric vector
pretty_symmetric <- function(vec) {
  if (any(vec < 0)) {
    stop("`vec` cannot contain 0 or negative numbers.")
  }

  rvec <- pretty(range(vec))
  lvec <- sort(-rvec)[1:length(rvec) - 1]

  return(c(lvec, rvec))
}

#' Make five-year age categories pretty.
#'
#' @description pop_data five-year age categories are formatted as "0_4",
#' "5_9", ... , "75_79", "80_". This function reformats these categories as
#' "0-4", "5-9", ... , "75-79", "80+".
#'
#' @param agecat_vec A vector of strings specifying age categories.
#'
#' @returns A pretty vector of strings
pretty_agecats <- function(agecat_vec) {
  if (!all(grepl("(?<=\\d)_(?=\\d)|(?<=\\d)_$", agecat_vec, perl = TRUE))) {
    stop("`agecat_vec` is not in the right format. All numbers must be NUM<underscore>NUM or NUM<underscore>.")
  }

  vec <- gsub("(?<=\\d)_(?=\\d)", "-", agecat_vec, perl = TRUE) # for 0-4, 5-9
  vec <- gsub("(?<=\\d)_$", "+", vec, perl = TRUE) # for 100+

  return(vec)
}

#' Defines the plot themes for the mortality-related plot functions.
#'
#' @description This function charts the theme for the mortality plots in OSDS.
#'
#' @import ggplot2
#'
#' @author Britta Schumacher
#'
#' @importFrom ggplot2 theme
theme_plot <- function(...) {
  theme(

    plot.title.position = "plot",
    #text = element_text(family = "Lato"),

    # background colors
    plot.background = element_rect(fill = "transparent",
                                   color = NA),
    panel.background = element_rect(fill = "transparent",
                                    color = NA),
    legend.background = element_rect(fill = "transparent",
                                     color = NA),

    # titles
    legend.title = element_blank(),
    legend.text = element_text(size = 12,
                               color = "black",
                               face = "plain"),
    plot.title = element_text(size = 14,
                              color = "black",
                              face = "bold",
                              lineheight = 1.2),
    plot.subtitle = element_text(size = 14,
                                 color = "black"),
    axis.title.x = element_text(size = 12,
                                color = "black",
                                face = "bold"),
    axis.title.y = element_text(size = 12,
                                color = "black",
                                face = "bold"),
    axis.text.x = element_text(size = 8.5,
                               color = "black"),
    axis.text.y = element_text(size = 9.5,
                               color = "black"),
    axis.ticks.y = element_blank(),
    ...
  )
}
