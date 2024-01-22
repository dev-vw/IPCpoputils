# Vania Wang
# 2023.07.17

#
# Pivoting IDB subnational datasets from wide to long
#
# Adapted with minor changes from code written by Vania Wang
#

# load packages ---
library(tidyverse)
library(here)
library(readxl)

setwd('C:/Users/wang0408/projects/ogac_requests/PopPyramids/data/raw/n_drive_data/')

# Set Country -----
country <- "Malawi"

# Load dataset ----
dat <- read_excel(paste0('data/raw/ndrive_data/', country, ".xlsx"))

# Remove unwanted columns ----
dat <- dat %>% dplyr::select(-c("GENC":"POP"))
dat <- dat %>% select(-starts_with(c("MPOP", "FPOP")))

# How many Admin levels are there? ----
max_adm <- max(dat$ADM_LEVEL)

# Function to create list of dfs, each containing area
# names and geomatch codes.
adm_name_gen <- function(n) {
  print(paste0('Processing ADM', n, ' names'))
  df <- dat[dat$ADM_LEVEL == n, ] %>% 
    group_by(GEO_MATCH, AREA_NAME) %>% 
    summarize()
  
  return(df)
}

# Apply the above function to every possible ADM level
adm_num <- c(0:max_adm)
adm_names_list <- lapply(adm_num, function(n) adm_name_gen(n))

# Correctly subsets GEOMATCH string
generate_geomatch_vecs <- function(vec, i) {
  vec_split <- str_split(dat$GEO_MATCH, '_')
  vec_sub <- lapply(vec_split, function(vec) vec[1:i])
  vec_cat <- unlist(lapply(vec_sub, function(vec) paste(vec, collapse = '_')))
  return(vec_cat)
}

# In dat, create new columns called ADM[NUM]_GEO_MATCH,
# where NUM ranges from 0 to 8
for (i in adm_num) {
  end_index = i+1
  print(end_index)
  vec <- unlist(generate_geomatch_vecs(dat$GEO_MATCH, end_index))
  dat[ , paste0('ADM', i, '_GEO_MATCH')] <- vec
}

# Select for rows of pop numbers for the max adm level only
wide_dat <- dat[dat$ADM_LEVEL == 2, ] %>% select(-ADM_LEVEL)

# The area name for ADM 0 should all be the same...the 
# country name
wide_dat$AREA_NAME_ADM0 <- adm_names_list[[1]]$AREA_NAME

# Redefine the adm numbers to loop over, and iteratively
# join wide_dat with a df containing ADM names at a given
# ADM level
adm_num <- c(1:max_adm)
for (i in adm_num) {
  col_to_joinx <- paste0('ADM', i, '_GEO_MATCH')
  col_to_joiny <- 'GEO_MATCH'
  print(adm_names_list[[i+1]])
  
  wide_dat <- left_join(wide_dat,
                        adm_names_list[[i+1]],
                        by = setNames(col_to_joiny, col_to_joinx),
                        suffix = c('', paste0('_ADM', i)))
  
}

# Make data long again
pop_dat_long <- gather(wide_dat, POP_CAT, POP_TOTAL, POP0_4:POP100_)
pop_dat_long$SEX <- gsub("POP.*$", "", pop_dat_long$POP_CAT)
pop_dat_long$AGE_CAT <- gsub(".POP", "", pop_dat_long$POP_CAT)
pop_dat_long <- dplyr::select(pop_dat_long, -POP_CAT)

# remove unnecessary columns
clean_dat <- 
  pop_dat_long %>% dplyr::select(-c("GEO_MATCH":"AREA_NAME"),
                                 -starts_with("ADM"))

colnames(clean_dat) <-  
  sub("AREA_NAME_", "", colnames(clean_dat))

clean_dat <- 
  clean_dat %>% 
  discard(~all(is.na(.) | . == ""))

# final <-
#   clean_dat %>% 
#   relocate("YR", "SEX", "AGE_CAT", "POP_TOTAL", .after = last_col())

final <- clean_dat
# ----- FILTER and SELECT DATA -----
# AGE_CAT: "0_4"   "5_9"   "10_14" "15_19" "20_24" "25_29" "30_34"
#          "35_39" "40_44" "45_49" "50_54" "55_59" "60_64" "65_69"
#          "70_74" "75_79" "80_84" "85_89" "90_94" "95_99" "100_"
# SEX: "F" "M"
# YR: "2000" to "2040"

# final <- filter(final,
#                 YR == 2023)
#                YR %in% (2020:2025)
write_csv(final,
          paste("data/raw/ndrive_data/idb_subnational_total_",
                country,
                ".csv",
                sep=""))

## ---- merging N drive country-files all files into one
ndrive_data <- list.files(path = "data/raw/ndrive_data/",
                          pattern = "idb_subnational_total_*", 
                          full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

write_csv(ndrive_data, 
          "data/clean/ndrive_clean_subnation_TOTAL.csv")
