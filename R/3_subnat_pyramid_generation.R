library(tidyverse)
library(ggplot2)
library(lemon)
library(gridExtra)

# ----------------------------------------------------
# ----- Import Youth Analysis (SUBNATIONAL) Data -----
# ----------------------------------------------------
youth_dat <- read.csv('data/clean/youthanalysis_data/youthdat_subnational.csv')

# ---------------------------------------------
# ----- Cleaning for FOR NDRIVE SITE DATA -----
# ---------------------------------------------
countryData <- read.csv("data/clean/ndrive_clean.csv") 

# ----- Some extra cleaning, gahhhh...
names(countryData) <- c('Country', 'ADM1', 'YR', 'Sex', 'AGE', 'POP')

countryData <- countryData %>%
  mutate(
    Sex = case_when(
      (Sex == "M") ~ "Male",
      (Sex == "F") ~ "Female",
      TRUE ~ as.character(Sex)
    ))

countryData$AGE <- as.factor(countryData$AGE)
levels(countryData$AGE) <- sub("_", "-", levels(countryData$AGE))

start_seq <- seq(0, 95, 5)
end_seq <- seq(5, 100, 5) - 1
level_vec <- paste(start_seq, end_seq, sep = "-")

countryData$AGE <- recode(countryData$AGE,
                          `100-` = '100+')

countryData$AGE <- factor(countryData$AGE,
                          levels = c(level_vec, "100+"))

# remove 70+ age groups
elderly_factors <- c("70-74", "75-79", "80-84",
                     "85-89", "90-94", "95-99",
                     "100+")

countryData <- countryData %>% 
  filter(!countryData$AGE %in% elderly_factors)

# ---------------------------------------------
# ----- Cleaning for FOR SUBNAT SITE DATA -----
# ---------------------------------------------
countryData <- read.csv("data/clean/subnatsite_clean.csv")

# ----- Some extra cleaning, gahhhh...
names(countryData) <- c('Country', 'ADM1', 'Sex', 'AGE', 'YR', 'POP')

countryData <- countryData %>%
  mutate(
    Sex = case_when(
      (Sex == "M") ~ "Male",
      (Sex == "F") ~ "Female",
      TRUE ~ as.character(Sex)
    ))

countryData$AGE <- as.factor(countryData$AGE)
levels(countryData$AGE) <- sub("_", "-", levels(countryData$AGE))

start_seq <- seq(0, 75, 5)
end_seq <- seq(5, 80, 5) - 1
level_vec <- paste(start_seq, end_seq, sep = "-")

countryData$AGE <- recode(countryData$AGE,
                          `80-` = '80+')

countryData$AGE <- factor(countryData$AGE,
                          levels = c(level_vec, "80+"))

# remove 70+ age groups
elderly_factors <- c("70-74", "75-79", "80+")

countryData <- countryData %>% 
  filter(!countryData$AGE %in% elderly_factors)

# -------------------------------
# ----- plotting automation -----
# -------------------------------
pop_pyramid <- function(country_data, country) {
  popPyrTitle <- paste("Population Pyramid",
                       str_to_title(country),
                       "2023")

  adm1_names <- unique(country_data$ADM1)

  plots <- lapply(adm1_names, function(name) {
    gen_individual_pyrplot(country, country_data %>% filter(ADM1 == name),
                           name)
  })

  print('Saving plots...')

  ggsave(
    filename = paste0("deliverables/subnational/", country, "_adm1.pdf"),
    plot = marrangeGrob(plots, nrow = 2, ncol = 1, top = NULL),
    width = 8.5,
    height = 11,
    units = "in"
  )
}

pretty_symmetric <- function(numcol) {
  rvec <- pretty(range(numcol))
  lvec <- sort(-rvec)[1:length(rvec)-1]
  return(c(lvec, rvec))
}

gen_bulge_blurb <- function(country, geo_name) {
  is_bulge <- youth_dat$is_youthbulge[youth_dat$Country == country & youth_dat$ADM1_name == geo_name]
  bust_or_bulge <- youth_dat$bulge_or_bust[youth_dat$Country == country & youth_dat$ADM1_name == geo_name]
  percent_youth <- paste0(round(100 * youth_dat$propYouth_2023[youth_dat$Country == country & youth_dat$ADM1_name == geo_name],
                                digits = 1), "%")
  pointChange <- round(youth_dat$pointChange[youth_dat$Country == country & youth_dat$ADM1_name == geo_name],
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
                         "of its over-15 population.")
  
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

gen_individual_pyrplot <- function(adm0_name, adm1_data, adm1) {
  blurb <- gen_bulge_blurb(adm0_name, adm1)
  pop_range_seq <- pretty_symmetric(adm1_data$POP/1000)

  ggplot(data = adm1_data, 
       mapping = aes(x = ifelse(test = Sex == "Male", 
                                yes = -POP/1000, 
                                no = POP/1000), 
                     y = AGE, 
                     fill = Sex)) +
    geom_col() +
    scale_x_symmetric(labels = abs, 
                      breaks = pop_range_seq) +
    ggtitle(adm1) +
    labs(x = "Population") +
    theme(legend.position = "none") +
    ylab("Age Categories, Five-year age groups") +
    xlab("Population (in thousands)") +
    labs(subtitle = "Source: U.S. Census Bureau | International Programs Center | International Database",
         caption = blurb)
}

# test
gen_individual_pyrplot('TANZANIA', 
                       countryData %>% filter(ADM1 == 'IRINGA'), 
                       'IRINGA')

# actual thing, run and save
country_names <- unique(countryData$Country)

lapply(country_names, function(country) {
  pop_pyramid(countryData %>% filter(Country == country),
              country)
})
