pop_data <- zambia
adm_sf <- zambia_adm2

joined_df <- join_data_to_shp(pop_data, adm_sf, adm_level = "ADM2", yr = 2015)

plot_chlor(joined_df,
           "Zambia",
           2015)
