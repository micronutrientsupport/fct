

library(tidyverse)

library(tidyverse)



#Loading Food Balance Sheet (new)


fbs <- read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv")) %>% 
  dplyr::select(-X)


#fixing original names 
fbs$original_id[fbs$original_name == "citrus, other"] <- "2614"

fbs$original_id[fbs$original_name == "marine fish, other"] <- "2764"

#Checking duplicates

fbs %>% group_by(country_id, date_consumed) %>% 
  count(original_id) %>% arrange(desc(n))

#
#2611            2
#2614            2
#2763            2
#2764            2
#2805            2


fbs %>% filter(original_id == "2611")

#removing duplicates from citrus and oranges
fbs <- fbs %>% distinct() 

fbs %>% group_by(country_id, date_consumed) %>% 
  count(original_id) %>% arrange(desc(n))

#2763            2
#2764            2
#2805            2

fbs %>% filter(original_id == "2763")

fct %>% filter(food_genus_id == "1532.01") #marine
fct %>% filter(food_genus_id == "1527.01") #pelagic
fct %>% filter(food_genus_id == "23161.01.01") #rice
fct %>% filter(food_genus_id == "23161.02.01") #rice, local

#removing wrongly coded items.
fbs <- fbs %>% filter(!(food_genus_id == "1532.01" & original_id == "2763")) %>% 
 filter(!(food_genus_id == "1527.01" & original_id == "2764")) %>% 
 filter(!(food_genus_id == "23161.01.01" & original_id == "2805"))  


fbs %>% select(1,4:5) %>%  distinct() %>% 
  left_join(., dictionary.df, by = c("food_genus_id" = "ID_3")) %>% 
  filter(is.na(FoodName_3)) %>% distinct()


readr::write_excel_csv2(fbs, here::here("output", "MAPS_FBS_2014-2018_v2.0.csv"))
readr::write_csv(fbs, here::here("output", "MAPS_FBS_2014-2018_v2.0.csv"))


fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.0.csv")) 

fbs %>% select(1,4:5) %>%  distinct() %>% 
  left_join(., dictionary.df, by = c("food_genus_id" = "ID_3")) %>% 
  filter(is.na(FoodName_3)) %>% distinct()

fbs %>% select(1,4:5) %>% filter(!is.na(food_genus_id)) %>% distinct() %>% 
  left_join(., MAPS_ken) %>% 
  filter(is.na(original_food_id)) %>% distinct()

