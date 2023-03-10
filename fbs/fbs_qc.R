



# Loading the data

fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.0.csv")) 
source("MAPS_Dictionary-Protocol.R")
source(here::here("FCTs", "kenfct.R"))
source(here::here("FCTs", "wafct.R"))

names(fbs)
str(fbs)

subset(dictionary.df, ID_2 == "21121")
# Checking countries values
unique(fbs$country_id)

country <- "ZAF"
all_variables <- names(fbs)[c(1:5,7)]

country_fbs <- subset(fbs, country_id %in% country) %>% group_by(across(all_variables)) %>% 
  summarise(mean_supply = round(mean(amount_consumed_in_g), 2)) %>% arrange(desc(mean_supply))

#write.csv(country_fbs, paste0(country, "_FBS_2014_2018.csv"))

#Checking dictionary ID matches
country_fbs %>% 
  left_join(., dictionary.df, by = c("food_genus_id" = "ID_3")) %>% 
  filter(is.na(FoodName_3)) %>% distinct()

#Checking fbs w/o a match in KE18
country_fbs %>% 
  left_join(., MAPS_ken, by = "food_genus_id") %>% 
  filter(is.na(original_food_id)) 
