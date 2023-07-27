
#Loading libraries
library(dplyr)


#Load FBS matched to dictionary (most recent one)
fbs_file <- sort(list.files(here::here("output") , "MAPS_FBS_2014-2018_v"), # identifying most recent one
             decreasing = T)[1]

fbs <- read.csv(here::here("output", fbs_file)) # loading the data

#Load FCTs matched to dictionary (most recent one)
file <- sort(list.files(here::here("inter-output") , "FCTs_dict_compiled_v"), # identifying most recent one
             decreasing = T)[1]

fct_dict <- read.csv(here::here("inter-output", file)) %>%  # loading the data
              dplyr::rename(food_genus_id = "ID_3") # changing name

names(fct_dict)

subset(fct_dict, fct_source = "US19", select = c(VITA_RAEmcg))

# Getting counrties id 
unique(fbs$country_id)

# Selecting country of interest
country <- "GHA"
all_variables <- names(fbs)[c(1:5,7)] # And variables of interest

# Getting the country mean supply
country_fbs <- subset(fbs, country_id %in% country) %>%
  group_by(across(all_variables)) %>% 
  summarise(mean_supply = round(mean(amount_consumed_in_g), 2)) %>% 
  arrange(desc(mean_supply))

# Matching with FCT by priority FCT
fct1 <- "WA19"
fct2 <- "KE18"
fct3 <- "UK21"
fct4 <- "US19"

nutrient <- "VITA_RAEmcg" # Nutrient of interest

# Matching with the priority 1 

matches1 <-  country_fbs %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct1), 
            by = "food_genus_id") %>% 
  filter(!is.na(!!sym(nutrient))) 

# Checking fbs w/o a match in fct1 & matches in fct priority 2
matches2 <- country_fbs %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct1), 
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient)))  %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct2),
            by = "food_genus_id") %>% 
  filter(!is.na(!!sym(nutrient))) 

# Checking fbs w/o a match in fct1 & matches in fct priority 2
matches3 <- country_fbs %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct1), 
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient)))  %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct2),
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient))) %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct3),
            by = "food_genus_id") %>% 
  filter(!is.na(!!sym(nutrient))) 

# Checking fbs w/o a match in fct1 & matches in fct priority 2
matches4 <- country_fbs %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct1), 
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient)))  %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct2),
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient))) %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct3),
            by = "food_genus_id") %>% 
  filter(is.na(!!sym(nutrient))) %>% 
  dplyr::select(1:ncol(country_fbs)) %>% 
  left_join(., fct_dict %>% dplyr::filter(source_fct %in% fct4),
            by = "food_genus_id") %>% 
  filter(!is.na(!!sym(nutrient))) 

data.df <- bind_rows(matches1, matches2, matches3, matches4)


data.df %>% dplyr::select(1:ncol(country_fbs), nutrient, 
                          Edible_factor_in_FCT, WATERg) %>%
  write.csv(., here::here("inter-output", paste0(country, "_", fbs_file)))




