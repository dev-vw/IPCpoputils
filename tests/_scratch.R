pop_data <- zambia
adm_sf <- zambia_adm4

filtered_pop_data <- zambia_2015_4 %>% group_by(ADM4) %>% summarize(POP_TOTAL = sum(POP_TOTAL))

joined_df <- join_data_to_shp(pop_data, adm_sf, adm_level = "ADM4", yr = 2015)

