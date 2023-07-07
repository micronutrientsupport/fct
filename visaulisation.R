




col_names <- c("fdc_id",
               "food_desc",
               "source_fct",
               "nutrient_data_source",
               "WATERg",
               "VITA_RAEmcg", 
                 "FOLmcg", 
               "VITB6_mg_std",
               "VITB12mcg",
               "FEmg",
               "ZNmg", 
               "SEmcg",
               "IDmcg")

MAPS_output %>% 
  select(c(8, 23:46))%>% 
  filter(fct_name %in% c("KE18", "LS06", "MW19", "WA19")) %>% 
  naniar::gg_miss_fct(., fct = fct_name)  +
  labs( x= "", y= "") +
  theme(
    axis.text.y = element_text(size=25),
    axis.text.x = element_text(size=22), 
    legend.text = element_text(size=18),
    legend.title = element_text(size=22) 
  )
