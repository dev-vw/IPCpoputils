library(ggplot2)
library(tidyverse)
library(lemon)
library(scales)

youth_dat <- read.csv("data/clean/youthanalysis_data/youthdat_national.csv")

file0 <- "data/clean/idb.csv"
file1_idb <- "data/clean/idb_other3_clean.csv"

nationalData <- read.csv(file1_idb)

gen_bulge_blurb <- function(country, geo_name) {
  is_bulge <- youth_dat$is_youthbulge[youth_dat$Country == geo_name]
  bust_or_bulge <- youth_dat$bulge_or_bust[youth_dat$Country == geo_name]
  percent_youth <- paste0(round(100 * youth_dat$propYouth_2023[youth_dat$Country == geo_name],
                                digits = 1), "%")
  pointChange <- round(youth_dat$pointChange[youth_dat$Country == geo_name],
                       digits = 2)
  
  end_year <- 2023
  
  if (country %in% c("TANZANIA", "SOUTH AFRICA")) {
    start_year <- 2015
  } else {
    start_year <- 2013
  }
  
  geo_name <- str_to_title(geo_name)
  
  percent_blurb <- paste("Youth aged 15-24 accounted for",
                         percent_youth,
                         "of its over 15 population.")
  
  pointChange_blurb <- paste("Between", start_year, "and", end_year,
                             "the point change in relative youth cohort was",
                             paste0(pointChange, "."))
  
  if (is_bulge) {
    if (bust_or_bulge == "BULGE") {
      blurb <- paste(paste0("**", geo_name, " is experiencing a youth bulge event.**\n"), percent_blurb, 
                     "\n", pointChange_blurb)
    } else {
      blurb <- paste(paste0("**", geo_name, " is experiencing a youth bust event.**\n"), percent_blurb, 
                     "\n", pointChange_blurb)
    }
  } else {
    blurb <- paste(paste0("**", geo_name, " is not experiencing a youth bulge or bust event.**\n"), percent_blurb,
                   "\n", pointChange_blurb)
  }
  return(blurb)
}

pretty_symmetric <- function(numcol) {
  rvec <- pretty(range(numcol))
  lvec <- sort(-rvec)[1:length(rvec)-1]
  return(c(lvec, rvec))
}

gen_individual_pyrplot_national <- function(country_data, name, has_blurb = FALSE) {
  if (has_blurb) {
    blurb <- gen_bulge_blurb(name, name)    
  } else {
    blurb = NULL
  }
  
  pop_range_seq <- pretty_symmetric(country_data$POP/1000)
  print(pop_range_seq)

  ggplot(data = country_data, 
         mapping = aes(x = ifelse(test = Sex == "Male", 
                                  yes = -POP/1000, 
                                  no = POP/1000), 
                       y = as.factor(AGE), 
                       fill = Sex)) +
    geom_col() +
    scale_x_symmetric(labels = abs,
                      breaks = pop_range_seq) +
    scale_y_discrete(breaks = as.character(seq(0, 75, 5))) +
    ggtitle(name) +
    theme(legend.position = "none") +
    ylab("Age, One-year age groups") +
    xlab("Population (in thousands)") +
    labs(subtitle = "Source: U.S. Census Bureau | International Programs Center | International Database",
         caption = blurb)
}

# the 'sex' variable is uppercase or titlecase depending on dataset
nationalData <- nationalData %>%
  mutate(
    Sex = case_when(
      (Sex == "M") ~ "Male",
      (Sex == "F") ~ "Female",
      TRUE ~ as.character(Sex)
    )) 

# for continuous age
nationalData <- nationalData %>%
  filter(Age <= 70)

# depending on column order, choose one of the following
# file 0
names(nationalData) = c('Country', 'Sex', 'AGE', 'POP')

# file 1
names(nationalData) = c('Country', 'AGE', 'Sex', 'POP')

countries <- unique(nationalData$Country)

# test with Ethiopia and Cameroon
gen_individual_pyrplot_national(nationalData %>% filter(Country == "ETHIOPIA"), 
                                "ETHIOPIA")
gen_individual_pyrplot_national(nationalData %>% filter(Country == "Cameroon"), 
                                "Cameroon")

# the actual thing, save plots into a list
graphics <- lapply(countries, function(country) 
  gen_individual_pyrplot_national(nationalData %>% filter(Country == country), 
                                  country))

# now, save plots into disk
names(graphics) <- countries

lapply(names(graphics), function(pic) {
  ggsave(filename = paste0("deliverables/national/", pic, "_national.pdf"),
         plot = graphics[[pic]],
         device = "pdf",
         width = 8.5,
         height = 11, 
         units = "in",
         limitsize = FALSE)
})

# individual export engine
ggsave(filename = paste0("deliverables/national/cameroon_national.png"),
       plot = pic,
       device = "png",
       width = 8.5,
       height = 11, 
       units = "in",
       limitsize = FALSE)
 