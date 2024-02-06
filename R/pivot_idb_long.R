#' @description
#'
#' @author Vania Wang
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom dplyr select left_join group_by summarize
#' @importFrom tidyr gather
#' @importFrom purrr discard
#' @importFrom stringr str_split
#'
#' @param country Case sensitive name of file name (excepting file extension).
#' @param datadir Relative or absolute path.
#' @param filetype Default value is `.csv`
#' @param skip Default value is `1`
#' @param is_legacy Default value is `FALSE`
#'
#' @examples
#' clean_idb("tanzania", "data-raw/", filetype = ".xlsx", is_legacy = TRUE)
#' clean_idb("kenya", "data-raw/", skip = 1)
#'
#' @returns A data frame.
#'
#' @export

clean_idb <- function(country,
                      datadir,
                      filetype = ".csv",
                      skip = 0,
                      is_legacy = FALSE) {

  # import file name
  if (filetype == ".csv") {
    dat <- readr::read_csv(paste0(datadir, country, filetype), skip = skip)
  } else if (filetype == ".xlsx") {
    dat <- readxl::read_excel(paste0(datadir, country, filetype), skip = skip)
  } else {
    stop("Error: input filetype is unrecognized")
  }

  # control structure for legacy/current versions
  if (is_legacy) {

    # remove unwanted columns
    dat <- dat %>% dplyr::select(-c("GENC":"POP_DENS"),
                                 -c("MPOP", "FPOP", "POP"))

    # append a 'B' in front of POP columns
    colnames(dat)[grepl("^POP*", colnames(dat))] <- paste0("B", colnames(dat)[grepl("^POP*", colnames(dat))])

    # how many adm levels are there?
    max_adm <- max(dat$ADM_LEVEL)

    # apply the above function to every possible ADM level
    adm_num <- c(0:max_adm)
    adm_names_list <- lapply(adm_num, function(n) adm_name_gen(n))

    # In dat, create new columns called ADM[NUM]_GEO_MATCH,
    # where NUM ranges from 0 to max_adm
    for (i in adm_num) {
      end_index = i+1
      print(end_index)
      vec <- unlist(generate_geomatch_vecs(dat$GEO_MATCH, end_index))
      dat[ , paste0('ADM', i, '_GEO_MATCH')] <- vec
    }

    # Select for rows of pop numbers for the max adm level only
    # wide_dat <- dat[dat$ADM_LEVEL == max_adm, ]
    wide_dat <- dat

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
      purrr::discard(~all(is.na(.) | . == ""))

    start <- seq(0, 95, 5)
    end <- seq(4, 99, 5)
    labels <- c(paste0(start, "_", end), "100_")

    clean_dat$AGE_CAT <- as.factor(clean_dat$AGE_CAT)
    clean_dat$AGE_CAT <- factor(clean_dat$AGE_CAT, levels = labels)

    final <-
      clean_dat %>%
      relocate("YR", "SEX", "AGE_CAT", "POP_TOTAL", .after = last_col())

    return(final)

  } else {
    # remove unwanted columns
    dat <- dat %>% dplyr::select(-c("USCBCMNT":"NSO_NAME"),
                                 -AREA_NAME,
                                 -GEO_MATCH)
    # remove columns with all NAs
    dat <- dat[, colSums(is.na(dat)) != nrow(dat)]

    # wide to long using gather
    dat <- dat %>% tidyr::gather(SEX_AGE_YR, POP_TOTAL, BTOTL_2000:F80PL_2025)

    # split the SEX_AGE_YR column into individual columns for each attribute
    dat <- dat %>%
      separate(col = SEX_AGE_YR,
               into = c('SEX_AGE', 'YR'),
               sep = '_',) %>%
      separate(col = SEX_AGE,
               into = c('SEX', 'AGE_CAT'),
               sep = 1) %>%
      filter(AGE_CAT != 'TOTL')

    # make consistent AGE groups with new IDB version data:
    # change factor names of AGE_CAT to cats used by the current IDB version (2023)
    dat$AGE_CAT <- as.factor(dat$AGE_CAT)
    levels(dat$AGE_CAT) <- c('0_4', '5_9', '10_14',
                             '15_19', '20_24', '25_29',
                             '30_34', '35_39', '40_44',
                             '45_49', '50_54', '55_59',
                             '60_64', '65_69', '70_74',
                             '75_79', '80_')

    print(levels(dat$AGE_CAT))
    # change name of CNTRY_NAME column
    names(dat)[names(dat) == 'CNTRY_NAME'] <- 'ADM0_NAME'

    # colnames for ONLY the relevant ADM levels
    max_adm <- max(dat$ADM_LEVEL)
    adm_names <- sapply(0:max_adm, function(num) paste0("ADM", num, "_NAME"))

    # select relevant ADM data
    dat <- dat %>% select(ADM_LEVEL, !!adm_names, YR, SEX, AGE_CAT, POP_TOTAL)

    # rearrange columns
    final <- dat %>%
      relocate("YR", "SEX", "AGE_CAT", "POP_TOTAL", .after = last_col())

    # get rid of _NAME suffic after ADM levels
    colnames(final) <- gsub("_NAME", "", colnames(final))

    return(final)
  }
}

