


library(tidyverse)


fbs <- read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv")) %>% 
  dplyr::select(-c(food_genus_confidence, X)) 

region <- "Western"

fct <- read.csv(here::here("output", paste0("MAPS_", region, "-Africa_v1.8.csv"))) %>% 
  dplyr::select(-food_genus_confidence) %>% distinct() 




fct %>% 
  group_by(original_food_name) %>% count() %>% arrange(desc(n))

fct %>% distinct() %>% 
  group_by(food_genus_id) %>% count() %>% arrange(desc(n))


fct %>% filter(food_genus_id == "1532.01") #marine
fct %>% filter(food_genus_id == "1527.01") #pelagic
fct %>% filter(food_genus_id == "23161.01.01") #rice
fct %>% filter(food_genus_id == "23161.02.01") #rice

fbs %>% filter(original_id == "2805")

fbs$original_id[fbs$original_name == "citrus, other"] <- "2614"

fbs$original_id[fbs$original_name == "marine fish, other"] <- "2764"

country_iso <- "AGO"
year  <- "2018"
mn <- paste0("ca", "_in_mg")

# original_id  n
#       2611   4
#       2763   4
#       2805   2

fbs %>% distinct() %>% group_by(country_id, date_consumed) %>% 
  count(original_id) %>% arrange(desc(n))

allocation <- fbs %>% group_by(country_id, date_consumed) %>% 
  count(original_id) %>% arrange(desc(n)) %>% rename(allocation = "n")

fbs %>% left_join(., allocation) %>% 
  filter(country_id == country_iso & date_consumed == year) %>% 
  mutate(amount_in_g = amount_consumed_in_g/allocation) %>% 
  left_join(., fct, by = "food_genus_id") %>% 
  mutate(se_consumed =  amount_in_g*se_in_mcg/100) %>%
  group_by(original_name) %>% summarise(se = sum(se_consumed)) %>%  
  arrange(desc(se)) %>% slice(1:20)

fbs %>% filter(country_id == country_iso & date_consumed == year) %>% 
  group_by(original_name) %>% count() %>% arrange(desc(n))

fbs %>% filter(country_id == country_iso & date_consumed == year) %>% 
  group_by(original_name) %>% count() %>% arrange(desc(n))

fbs %>% filter(country_id == country_iso & date_consumed == year) %>% 
  left_join(., fct, by = "food_genus_id") %>% 
  mutate(ca_consumed =  amount_consumed_in_g*ca_in_mg/100) %>% 
  arrange(desc(ca_consumed)) %>% slice(1:20)


fbs %>% filter(country_id == country_iso & date_consumed == year) %>% 
  left_join(., fct, by = "food_genus_id") %>% 
  mutate(se_consumed =  amount_consumed_in_g*se_in_mcg/100) %>% 
  arrange(desc(se_consumed)) %>% slice(1:20)

fbs %>% filter(country_id == country_iso & date_consumed == year) %>% 
  left_join(., fct, by = "food_genus_id") %>% 
  mutate(se_consumed =  amount_consumed_in_g*se_in_mcg/100) %>%
  group_by(original_name) %>% summarise(se = sum(se_consumed)) %>%  
  arrange(desc(se)) %>% slice(1:20)


#testing nigeria data w/WAFCT

fbs_test <- read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv")) %>%
  select(-X) %>% filter(country_id == "NGA") %>% 
  distinct(food_genus_id, original_name)
#Joing FBS w/ WAFCT, 2019

fbs_test %>% left_join(., MAPS_wafct) %>% filter(!is.na(energy_in_kcal)) %>% count()