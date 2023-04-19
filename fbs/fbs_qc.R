



# Loading the data

fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.1.csv")) 
fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.1.1.csv")) 
source("MAPS_Dictionary-Protocol.R")
source(here::here("FCTs", "kenfct.R"))
source(here::here("FCTs", "wafct.R"))

names(fbs)
str(fbs)

subset(dictionary.df, ID_2 == "21121")
subset(dictionary.df, ID_3 == "2899.01.01")
subset(MAPS_output, food_genus_id == "21691.07.01")

# Checking countries values
unique(fbs$country_id)

country <- "UGA"
all_variables <- names(fbs)[c(1:5,7)]

country_fbs <- subset(fbs, country_id %in% country) %>% group_by(across(all_variables)) %>% 
  summarise(mean_supply = round(mean(amount_consumed_in_g), 2)) %>% arrange(desc(mean_supply))

#write.csv(country_fbs, paste0(country, "_FBS_2014_2018.csv"))

#Checking dictionary ID matches
country_fbs %>% 
  left_join(., dictionary.df, by = c("food_genus_id" = "ID_3")) %>% 
  filter(is.na(FoodName_3)) %>% distinct()

#Getting fbs w/ a match in KE18
ken <-  country_fbs %>% 
  left_join(., MAPS_ken, by = "food_genus_id") %>% 
  filter(!is.na(original_food_id)) 

#Checking fbs w/o a match in KE18 & matches in WA19
wa <- country_fbs %>% 
  left_join(., MAPS_ken, by = "food_genus_id") %>% 
  filter(is.na(original_food_id)) %>% 
  select(1:7) %>% 
  left_join(., MAPS_output, by = "food_genus_id") %>% 
  filter(!is.na(original_food_id)) 

country_fbs %>% 
  left_join(., MAPS_ken, by = "food_genus_id") %>% 
  filter(is.na(original_food_id)) %>% 
  select(1:7) %>% 
  left_join(., MAPS_output, by = "food_genus_id") %>% 
  filter(is.na(original_food_id)) %>% distinct(original_name)

ken$cu_in_mg <- as.numeric(ken$cu_in_mg)
ken$vitaminb6_in_mg <- as.numeric(ken$vitaminb6_in_mg)
ken$folicacid_in_mcg <- as.numeric(ken$folicacid_in_mcg)
ken$vitamind_in_mcg <- as.numeric(ken$vitamind_in_mcg)
ken$vitamine_in_mg <- as.numeric(ken$vitamine_in_mg)

ken %>% rbind(., wa) %>% 
  mutate(
  VitA = (mean_supply*vitamina_in_rae_in_mcg)/100) %>% 
  group_by(country_id) %>% 
  summarise(vita_mean = sum(VitA))
