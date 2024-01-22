#' @description
#'
#' @author Vania Wang
#' @importFrom readxl read_excel
#' @param fpath A file path string to the `.dp` input file.
#' @returns A data frame.
#' @export

# load packages ---
#library(tidyverse)
#library(readxl)

# LEGACY IDB VERSION

# Set Country -----
country <- "Tanzania"

# Load dataset ----
dat <- readxl::read_excel(paste0('data-raw/', country, ".xlsx"))

# Remove unwanted columns ----
dat <- dat %>% dplyr::select(-c("GENC":"POP_DENS"),
                             #                             -starts_with("POP"),
                             -c("MPOP", "FPOP", "POP"))

colnames(dat)[grepl("^POP*", colnames(dat))] <- paste0("B", colnames(dat)[grepl("^POP*", colnames(dat))])

# How many Admin levels are there? ----
max_adm <- max(dat$ADM_LEVEL)

# Function to create list of dfs, each containing area
# names and geomatch codes.
adm_name_gen <- function(n) {
  print(paste0('Processing ADM', n, ' names'))
  df <- dat[dat$ADM_LEVEL == n, ] %>%
    dplyr::group_by(GEO_MATCH, AREA_NAME, ADM_LEVEL) %>%
    dplyr::summarize()

  return(df)
}

# Apply the above function to every possible ADM level
adm_num <- c(0:max_adm)
adm_names_list <- lapply(adm_num, function(n) adm_name_gen(n))

# Correctly subsets GEOMATCH string
generate_geomatch_vecs <- function(vec, i) {
  vec_split <- stringr::str_split(dat$GEO_MATCH, '_')
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
wide_dat <- dat[dat$ADM_LEVEL == max_adm, ]

# The area name for ADM 0 should all be the same...the
# country name
wide_dat$AREA_NAME_ADM0 <- adm_names_list[[1]]$AREA_NAME

# Redefine the adm numbers to loop over, and iteratively
# join wide_dat with a df containing ADM names at a given
# ADM level
adm_num = c(1:max_adm)
for (i in adm_num) {
  col_to_joinx <- paste0('ADM', i, '_GEO_MATCH')
  col_to_joiny <- 'GEO_MATCH'
  print(adm_names_list[[i+1]])

  wide_dat <- dplyr::left_join(wide_dat,
                               adm_names_list[[i+1]],
                               by = setNames(col_to_joiny, col_to_joinx),
                               suffix = c('', paste0('_ADM', i)))

}

# Make data long again
pop_dat_long <- tidyr::gather(wide_dat, POP_CAT, POP_TOTAL, BPOP0_4:FPOP100_)
pop_dat_long$SEX <- gsub("POP.*$", "", pop_dat_long$POP_CAT)
pop_dat_long$AGE_CAT <- gsub(".POP", "", pop_dat_long$POP_CAT)
pop_dat_long <- dplyr::select(pop_dat_long, -POP_CAT)

# remove unnecessary columns
clean_dat <-
  pop_dat_long %>% dplyr::select(ADM_LEVEL, YR,
                                 starts_with("AREA_NAME_"),
                                 POP_TOTAL, SEX, AGE_CAT)

colnames(clean_dat) <-
  sub("AREA_NAME_", "", colnames(clean_dat))

clean_dat <- clean_dat %>%
  dpurrr::discard(~all(is.na(.) | . == ""))



final <-
  clean_dat %>%
  relocate("YR", "SEX", "AGE_CAT", "POP_TOTAL", .after = last_col())

# -----------------

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

# Set Country -----
country <- "kenya"

# Load dataset ----
dat <- read_csv(paste0('data-raw/', country, ".csv"), skip = 1)

# Remove unwanted columns ----
dat <- dat %>% dplyr::select(-c("USCBCMNT":"NSO_NAME"),
                             #                             starts_with(c('M', 'F')),
                             #                            -starts_with('B'),
                             -AREA_NAME,
                             -GEO_MATCH)

dat <- dat[, colSums(is.na(dat)) != nrow(dat)]

# wide to long
dat <- dat %>% gather(SEX_AGE_YR, POP_TOTAL, BTOTL_2000:F80PL_2025)

# split the SEX_AGE_YR column into individual columns
# for each attribute
dat <- dat %>%
  separate(col = SEX_AGE_YR,
           into = c('SEX_AGE', 'YR'),
           sep = '_',) %>%
  separate(col = SEX_AGE,
           into = c('SEX', 'AGE_CAT'),
           sep = 1) %>%
  filter(AGE_CAT != 'TOTL')

# make consistent AGE groups with new N-drive data:
# change factor names of AGE_CAT to cats used by N-drive
# data
dat$AGE_CAT <- as.factor(dat$AGE_CAT)
levels(dat$AGE_CAT) <- c('0_4', '5_9', '10_14',
                         '15_19', '20_24', '25_29',
                         '30_34', '35_39', '40_44',
                         '45_49', '50_54', '55_59',
                         '60_64', '65_69', '70_74',
                         '75_79', '80_')

# change name of CNTRY_NAME column
names(dat)[names(dat) == 'CNTRY_NAME'] <- 'ADM0_NAME'

max_adm <- max(dat$ADM_LEVEL)
adm_names <- sapply(0:max_adm, function(num) paste0("ADM", num, "_NAME"))

dat <- dat %>% select(ADM_LEVEL, !!adm_names, YR, SEX, AGE_CAT, POP_TOTAL)

dat <- dat %>%
  relocate("YR", "SEX", "AGE_CAT", "POP_TOTAL", .after = last_col())

colnames(dat) <- gsub("_NAME", "", colnames(dat))

final <- dat

