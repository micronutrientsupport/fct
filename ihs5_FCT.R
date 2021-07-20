

library(tidyverse)



## loading data

ihs5 <- read.csv("ihs5-fct_v1.0.csv")

AHHA <- read.csv(here::here("data", "AHHA-FCT.csv"))

MAFOODS <- read.csv(here::here("output", "MAPS_MAFOODS_v1.4.csv"))

WAFCT <- read.csv(here::here("output", "MAPS_WAFCT_v1.2.csv"))


c("food_genus_description"    , "food_group"               ,  "food_subgroup" ,
  "food_genus_confidence"    ,  "fct_name"  , "energy_in_kj", 
  "nitrogen_in_g"       ,       "totalprotein_in_g"      ,    "totalfats_in_g" ,
  "saturatedfa_in_g"           "monounsaturatedfa_in_g"     "polyunsaturatedfa_in_g" ,
  "cholesterol_in_mg"          "carbohydrates_in_g"         "fibre_in_g"  ,
  "ash_in_g" , )

#popcorn - AHHA - 1009 



#mucuna - WAFCT - 03_059 and Joy et al., 
#unprocess -

 ihs5 %>% 
  relocate(food_genus_id, .after = "ihs5_fooditem") %>% 
  mutate(ref = ifelse(is.na(ref.x), ref.x.x, 
                      ifelse(is.na(ref), ref.x, ref))) %>% 
  select(-c("FoodName_3":"FoodName_2")) %>% 
  select(-ends_with("_code")) %>% 
   select(-ends_with("_fct")) %>% 
   select(-ends_with(".x")) %>% 
  filter(!food_genus_id %in% c("1530.06", "1321.02", "1322.02", "1324.02" )) %>% 
  mutate(food_genus_id = case_when(
    food_genus_id == "24230.03.03|24230.03.02" ~ "24230.03.02",
    food_genus_id == "1290.01.01|1290.01.02" ~ "1290.01.03",
    TRUE ~ food_genus_id)) %>% 
       count(ref_fctcode) %>% arrange(desc(n))
 
#We need to remove some items that are not well represented,
#those are food items with a genus code that did not corresponed to
#a food compo entry the list is a follow:
# "1530.06" ==  to orange, sweet potato
# "1321.02" == pomelo/grapefruit
# "1322.02" == limes,
# "1324.02" == mandarines
# 
# to be checked! 406 - other cultivated leafy
# 
# other issues is wrong coding at consumption level (i.e., applying differente
 #genus_id to same como item. that was the case for 24230.03.02 - maize beer), 
 #same happened w/ green maize
 
 #BEST TO CHECK ALL ITEMS AND MATCHES ONE BY ONE!!!
 

# 
# #
# #
 