pop_data <- zambia
pop_sf_zb <- zambia_adm4

joined_df <- join_data_to_shp(pop_data, pop_sf_zb, adm_level = "ADM4", yr = 2015)

plot_chlor(joined_df,
           "Zambia",
           2015)
