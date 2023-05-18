
library(dplyr)


# Loading the data

fbs <- read.csv(here::here("inter-output", "MAPS_FBS_2018-2020_v3.0.1.csv")) 

#fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.1.1.csv")) 

#MAPS_dict_fbs <- read.csv(here::here("output", "MAPS_FBS_2014-2018_v2.1.1.csv")) %>% 
 # select(original_id, food_genus_id, food_genus_confidence, country_id) %>% distinct()

#saveRDS(MAPS_dict_fbs, here::here("inter-output", "MAPS_dict_fbs_v2.1.1.rds"))

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

#source(here::here("FCTs", "kenfct.R"))
#source(here::here("FCTs", "wafct.R"))

#Load dictionary-to-FCTs-matches (can be found in the fct_dict.R)
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

dict_comp <-read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")

dict_comp %>% count(source_fct) 

names(dict_comp)
str(fbs)



## Check FBS dict codes w/ fct value

code <- fbs %>% filter(!is.na(food_genus_id)) %>% 
  distinct(food_genus_id) %>% left_join(., dict_comp, by = c("food_genus_id" = "ID_3")) %>% 
  filter(is.na(fdc_id)) %>% .[,1]

subset(dictionary.df, ID_1 == "2680")
subset(dictionary.df, ID_2 == "2680")
subset(dictionary.df, ID_3 %in% code) %>% distinct(ID_1)

subset(MAPS_output, food_genus_id == "21691.07.01")

# Checking countries values in FBS -----
unique(fbs$country_id)

country <- "UGA"
all_variables <- names(fbs)[c(1:5,7)]

country_fbs <- subset(fbs, country_id %in% country) %>% group_by(across(all_variables)) %>% 
  summarise(mean_supply = round(mean(amount_consumed_in_g), 2)) %>% arrange(desc(mean_supply))


subset(country_fbs, grepl("bovi|offa|poult|pig|goat", original_name,  ignore.case = TRUE))

subset(country_fbs, original_id %in% c("2731", "947", "867"))

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
  VitA = (mean_supply*vitamina_in_rae_in_mcg)/100) %>% View()

  group_by(country_id) %>% 
  summarise(vita_mean = sum(VitA))
  
  
  



## Checking SUA data -----
  
  # Load SUA data
  
  sua <- read.csv(here::here("inter-output", "MAPS_SUA_v1.0.0.csv"))
  names(sua)
  
  sua %>% left_join(., fct_dict, by = "ID_3") %>% 
    filter(!is.na(WATERg)) %>% distinct(Item)
  