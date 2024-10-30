# plot pop test
pop_data <- zambia
pop_sf_zb <- zambia_adm4

joined_df <- join_data_to_shp(pop_data, pop_sf_zb, adm_level = "ADM4", yr = 2015)

plot_chlor(joined_df,
           "Zambia",
           2015)

# plot mort test
library(readxl)

mw_mort <- read_excel("data-raw/mort-data/malawi.xlsx")

# test plot mort
# data read in and clean up
library(readxl)

mort <- readxl::read_excel("./data-raw/mort-data/malawi.xlsx",
                           sheet = 1)
mort <- tidyr::pivot_longer(mort,
                            3:10,
                            names_to = "metric",
                            values_to = "value")
mort <- mort |> dplyr::mutate(YR = substr(metric, nchar(metric) - 3, nchar(metric)),
                              metric = substr(metric, 1, nchar(metric) -5))
