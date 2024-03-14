




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


### 



desc1 <- "maize"
desc2 <- "grain"
desc3 <-  "green"
n.size <- 18

subset(fct_dict, grepl(desc1, food_desc, ignore.case = TRUE) &
         grepl(desc2, food_desc, ignore.case = TRUE) &
         !grepl(desc3, food_desc, ignore.case = TRUE),
       select = c(source_fct, fdc_id, food_desc, scientific_name, ID_3, WATERg, SEmcg)) %>% View() 
filter(!is.na(ID_3)) %>% left_join(., dictionary.df, by= "ID_3") %>% 
  ggplot(aes(FoodName_1, as.numeric(SEmcg), fill = FoodName_1)) + 
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, 
                     name = "", 
                     labels = c("Maize and products", 
                                "Millet and products", 
                                "Rice and products", 
                                "Sorghum and products")) +
  scale_x_discrete(labels = c("", 
                              "", 
                              "", 
                              "")) +
  geom_point(shape = 21, colour = "black", fill = "darkgrey", size =3) +
  theme_light() +
  labs(
    #  title = "Daily Se intake from maize in Malawi",
    y = "Se (mcg/100g)",
    x = ""
  ) +
  # scale_fill_discrete() +
  theme_light() +
  theme(
    # title = element_text(size = n2.size),
    #  axis.text.x = element_text(size = n.size),
    # axis.title.x = element_text(size = n.size),
    # axis.text.y  = element_text(size = n.size),
    legend.text =  element_text(size = n.size),
    legend.title =  element_text(size = n.size),
    legend.position = "bottom" 
  ) 