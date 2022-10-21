

library(tidyverse)
source(here::here("kenfct.R"))



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

#Added missing food dictionary code for "palm kernel"
fbs$food_genus_id[fbs$original_id == "2562"] <- "1491.02.01"
fbs$food_genus_confidence[fbs$original_id == "2562"] 
#Changed rice, imported for rice, local 23161.02.01
fbs$food_genus_id[fbs$food_genus_id == "23161.01.01"] <- "23161.02.01"
#Changed sugar cane to sugar cane, juice
fbs$food_genus_id[fbs$food_genus_id == "1802.01"] <- "1802.02"
#Changed sugar from beet and fructose to sugar
fbs$food_genus_id[fbs$food_genus_id == "1801.01"] <- "23520.01"
fbs$food_genus_id[fbs$food_genus_id == "23210.01.01"] <- "23520.01"
#Changing wine rose to red wine
fbs$food_genus_id[fbs$food_genus_id == "24212.02.03"] <- "24212.02.02"
#palm kernel (1491.02.01) - it won't be matched, but the amount consumed is zero
#see documentation
mean(fbs$amount_consumed_in_g[fbs$food_genus_id=="1491.02.01"])
#Correcting original_id marine fish, other 2763
#2764 = Marine Fish, Other #2763 = Pelagic Fish
fbs$original_id[fbs$original_name == "marine fish, other"] <- "2764"



subset(fbs, food_genus_id == "2413.01")
subset(fbs, original_id == "2763",
       select = c(original_name, food_genus_id)) %>% distinct()

subset(fbs, original_name == "marine fish, other",
       select = c(original_name, original_id, food_genus_id)) %>% distinct()

#Checking duplicated
fbs %>% select(1,4:5) %>% distinct() %>% 
  left_join(., MAPS_ken) %>% 
  filter(is.na(original_food_id)) %>%
  distinct() %>% count(food_genus_id) %>% arrange(desc(n))

#Checking fbs w/o a match in KE18
fbs %>% select(1,4:5) %>% distinct() %>% 
  left_join(., MAPS_ken) %>% 
  filter(is.na(original_food_id)) %>%
  distinct() %>% pull(food_genus_id)

#Checking fbs w/o a match in WA19
fbs %>% select(1,4:5) %>% distinct() %>% 
  left_join(., MAPS_wafct) %>% 
  filter(is.na(original_food_id)) %>%
  distinct() %>% pull(food_genus_id)

readr::write_csv(fbs, here::here("output", "MAPS_FBS_2014-2018_v2.1.csv"))

fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.0.csv")) 


names(fbs)

subset(fbs, str_detect(original_name, "apples and products"), 
       select = c(original_name, food_genus_id)) %>% distinct()
       