

library(tidyverse)


MAFOODS <- read.csv(here::here("MAPS_MAFOODS_v1.4.csv"))

recipe <- read.csv(here::here("data", "recipe-calculation.csv"))

NutRet <- read.csv(here::here("data", "NutrientRetention.csv"))

#Need to fill up the data gaps w/ different data sources

recipe <- recipe %>% left_join(., MAFOODS, 
                               by = c("ingr_code" = "original_food_id"))


#getting the igr_code of selenium missing values needed to complete 
#se values for the recipe

se.na <- recipe %>% filter(is.na(se_in_mcg)) %>% pull(ingr_code)

#OPTION ONE

#recipe %>% mutate(se_in_mcg = case_when(
 # ingr_code == "MW03_0059"~ 2.650, #KENFCT - 6022 (From AHHA)
  #ingr_code == "MW01_0024" ~ 3 , #LSOFCT -  11032 (Water 14))

#OPTION4
#recipe[recipe$ingr_code == "MW03_0059", "se_in_mcg"] <- 2.650

#OPTION3 

recipe.se <- tribble(
  ~ingr_code, ~se_in_mcg, ~fct_name, ~fct_id, ~water_in_g, ~RF, ~RF_citation, 
 "MW03_0059", 3 , "KENFCT", "6022", 86.3, 1, "KEN05_0001RF",
 "MW01_0024", 3 , "LSOFCT", "11032",  14, 1, "KEN01_0008RF",
  "MW03_0015", 23 , "KENFCT", "7011", 75.9, 1, "KEN07_0002RF",
 "MW08_0006", 1 , "KENFCT","11003", 0 , NA, NA, 
 "MW05_0004", 0.2828, "EJ", "EJ05_0004", 72, 1, "KEN04_0017RF",
  "MW01_0019",0.6912, "EJ", "EJ1_0029", 10, 1, "KEN01_0003RF")

recipe <- recipe %>% left_join(., recipe.se, 
                     by = c("ingr_code" = "ingr_code"))

#NEXT STEP IMPROVEMENT - Just create two columns with mutate as OPTION 1 
#Adding the code and FCT and then left_join all micronutrients and filled it
#as for the Kevin Matching script


recipe %>% left_join(., NutRet)

#DONT RUN - It provides wrong results - see below explanation

recipe %>%  mutate(se_in_mcg = ifelse(is.na(se_in_mcg.x), 
                            se_in_mcg.y*(100-moisture_in_g)/(100-water_in_g), 
                            se_in_mcg.x)) %>% 
  mutate_if(is.numeric,funs(.*ingr_g/100))

recipe %>%  mutate(se_in_mcg = ifelse(is.na(se_in_mcg.x), 
                                      se_in_mcg.y*(100-moisture_in_g)/(100-water_in_g), 
                                      se_in_mcg.x)) %>% 
 transform(across(is.numeric, funs(.*ingr_g/100)))

#There were two problems with getting recipe %
#one mutate acting over the columns it change the ingr_g data and it provided
#wrong values -> this was solved by using the lambda function()
#two mutate for is.numeric changed variables that shouldn't be included. 

recipe.ing <- recipe %>%  mutate(se_in_mcg = ifelse(is.na(se_in_mcg.x), 
                                                    se_in_mcg.y*(100-moisture_in_g)/(100-water_in_g), 
                                                    se_in_mcg.x)) %>% 
  mutate_at(vars(matches("_in_")), function(x) x * recipe$ingr_g / 100)



recipe.ing %>% group_by(food_code, food_name) %>% 
  summarise(across(is.numeric, sum)) %>% pull(food_code, se_in_mcg)

#NOTE: RECIPES NEED TO BE CHECKED AND CLOSER LOOK! MANY MN should be checked!!


