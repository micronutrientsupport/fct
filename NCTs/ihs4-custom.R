library(tidyverse)


ihs4.df <- read.csv(here::here("ihs4import.csv"))

source("dictionary.R")

ihs4.df %>% anti_join(wa_genus,
                      by = c("food_genus_id" = "ID_3"))


ihs4.df <- read.csv(here::here("data", "MAPS_customized-ihs4_v1.0.csv"))

food_genus_id <- c("21119.01.01",
                   "1324.02",
                   "1322.02", 
                   "24110.01")


ihs4.df %>% bind_rows(as.data.frame(food_genus_id)) %>% tail()



MAPS_ihs4.df <- ihs4.df %>%
  mutate(FCT = "ihs4-custom") %>% 
  mutate(food_genus_confidence = "NA",
         energy_in_kj = "NA", 
         totalprotein_in_g = "NA", 
         totalfats_in_g = "NA",
         saturatedfa_in_g = "NA", 
         monounsaturatedfa_in_g = "NA", 
         polyunsaturatedfa_in_g = "NA", 
         cholesterol_in_mg = "NA",
         carbohydrates_in_g = "NA", 
         fibre_in_g = "NA", 
         ash_in_g = "NA",
         p_in_mg = "NA", 
         folicacid_in_mcg = "NA", 
         vitamind_in_mcg = "NA",
        phyticacid_in_mg = "NA", 
        original_food_id = "NA", 
        nitrogen_in_g = "NA", 
        mn_in_mcg = "NA",
        pantothenate_in_mg = "NA",
        biotin_in_mcg = "NA"
  ) %>% 
  rename(
    original_food_name = "fooditem",
    fct_name = "FCT",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1", 
    data_reference_original_id = "ref",
    moisture_in_g = "WATER",
    energy_in_kcal = "ENERC1",
    ca_in_mg = "CA", 
    fe_in_mg = "FE",
    mg_in_mg = "MG",
    k_in_mg = "K",
    na_in_mg = "na", 
    zn_in_mg = "ZN", 
    cu_in_mg = "CU", 
    i_in_mcg = "ID", 
    se_in_mcg = "SE",
    vitamina_in_rae_in_mcg = "VITA_RAE", 
    thiamin_in_mg = "THIA",
    riboflavin_in_mg = "RIBF", 
    niacin_in_mg = "NIA", 
    vitaminb6_in_mg = "VITB6", 
    folate_in_mcg = "FOL",
    vitaminb12_in_mcg = "VITB12", 
    vitaminc_in_mg = "VITC",
    vitamine_in_mg = "VITE")

MAPS_ihs4.df %>% select(var.name) %>% 
  readr::write_excel_csv(., 
                         here::here('output', 'MAPS_ihs4-custom-fct_v0.3.csv')) #that f(x) is to 
                                #deal w/ special characters 

