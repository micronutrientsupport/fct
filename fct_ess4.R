################################################################################
#  
#
#      Script for matching ESS4 food list with food composition data
#
#
#  Data source citation: Central Statistical Agency of Ethiopia. 
#   Ethiopia Socioeconomic Survey (ESS4) 2018-2019. Public Use Dataset. 
#   Ref: ETH_2018_ESS_v01. Downloaded from
#   https://microdata.worldbank.org/index.php/catalog/3823 on 15/03/2022
#  
#   
#  
#  
#  
#
################################################################################



#0) Loading r packages

library(tidyverse)

source("fct_ess3.R")

#New dataset from 2018-2019.

ess4_food_list <- read.csv(here::here("inter-output", "eth_fct_match_v1.csv")) %>% 
   select(1, 2) %>% mutate(
      ref_fooditem = str_replace(item_code, "[:digit:]{2,3}\\.", "")) %>% 
   mutate_at("ref_fooditem", str_squish) %>% select(-item_code)


ess4_food <- ess4_food_list  %>%  left_join(., ess3_food_st) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   mutate(
      ref_foodid = case_when(
         str_detect(ref_fooditem, "Wheat") ~"2",
         str_detect(ref_fooditem, "Barley") ~ "3", 
         str_detect(ref_fooditem, "Green chili") ~  "141", 
         str_detect(ref_fooditem, "Red pepper") ~   "142",
         str_detect(ref_fooditem, "Injera") ~   "195",
         str_detect(ref_fooditem, "bread") ~   "196",
         TRUE ~ ref_foodid)) 


ess4_food <- ess4_food %>% filter(!is.na(ref_foodid), is.na(ID_3)) %>% select(1:3) %>% 
   left_join(., ess3_food_st %>% select(-ref_fooditem)) %>% 
   bind_rows(., ess4_food %>% filter(!is.na(ID_3))) 


#Assigning standard matching code (from dictionary code) to ess4

dictionary.df %>% filter(str_detect(FoodName_3, "beet"))
dictionary.df %>% filter(str_detect(FoodName_2, "other vegetables"))
dictionary.df %>% filter(ID_1 == "2645") %>% distinct(FoodName_2)
dictionary.df %>% filter(ID_2 == "1659")

ess4_food_st <- tribble(
  ~ref_foodid, ~ref_fooditem,   ~ID_3, ~confidence,
  
   "107",   "                                     Rice      ",  "23161.01.01", "m",
   "  108 ","                                          Oats ", "23140.07.01", "m", 
   #"  207", "                                        Vetch  ", "1709.01.01", "l", 
   "  207", "                                        Vetch  ",  "1702.01", "m", #change to horse bean 
   "  208", "                                    Fenugreek  ", "1699.07", "m", 
   "  209", "                                    mung bean  ", "1709.9.01", "m", 
   "  210", "                     Processed pulses (Shiro)  ", "23170.03.01", "l",
   "  303", "                                       SESAME  ",  "1444.01", "m", 
   "  304", "                                   Sun Flower  ",  "1445.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1215.01", "m", 
   "   406","                                         Garlic", "1252.01", "m", 
   "   407","                       Moringa/Shiferaw/Halloka", "1699.08", "m", 
   "   503","                                          Mango", "1316.01", "m", 
   "   504","                                         Papaya", "1317.01", "m", 
   "   505","                                        Avocado", "1311.01", "m", 
   "   608","                                         Carrot", "1251.01", "h", 
   "   609","                                       Beetroot", "1290.9.01", "m", 
   "   711","                                 Honey, natural", "2910.01", "m", 
   "   806","                                     Chat / Kat", NA, NA, 
   "   807","                                   Hops (gesho)","1699.09" , "m") %>% 
   mutate_all(., str_squish) %>% 
left_join(., dictionary.df %>% distinct()) 


#We are removing "others" because we are not matching those items
#We are adding the items new in ess4

ess4_food_list %>% left_join(., ess4_food) %>% filter(!is.na(ID_3)) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   rename(ref_foodid_ess3 = "ref_foodid", 
          ref_foodid = "fcode") %>% 
   mutate_at("ref_foodid", as.character) %>% 
   bind_rows(., ess4_food_st) %>% distinct(ref_foodid)

#Checking that we are not missing any code...
ess4_food_list %>% filter( !str_detect(ref_fooditem, "Other")) %>%  distinct(ref_foodid)


ess4_food_list <- ess4_food_list %>% left_join(., ess4_food) %>% filter(!is.na(ID_3)) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   rename(ref_foodid_ess3 = "ref_foodid", 
          ref_foodid = "fcode") %>% 
   mutate_at("ref_foodid", as.character) %>% 
   bind_rows(., ess4_food_st)



#Assigning 

#3) Food matching

#KENFCT, 2018
source("kenfct.R")

missig_values <-  ess4_food_list %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(energy_in_kcal)) %>% 
    select(1:2,4)

ess4_food_list %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(zn_in_mg)) %>% 
   select(1:2,4)

ess4_ken <- ess4_food_list %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
   filter(!is.na(energy_in_kcal)) %>% 
   select(1:2,4, original_food_id, original_food_name, fct_name, moisture_in_g,
          energy_in_kcal,vitamina_in_rae_in_mcg, zn_in_mg) 

dictionary.df %>% filter(ID_3 == "1441.01")

#WAFCT, 2019

source("wafct.R")

missig_values %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_wafct, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(energy_in_kcal)) %>% 
   select(1:2,4)

missig_values <- missig_values %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_wafct, by = c("ID_3" = "food_genus_id")) %>% 
   filter(!is.na(energy_in_kcal)) %>% 
   select(1:2,4, original_food_id, original_food_name, fct_name, moisture_in_g,
          energy_in_kcal,vitamina_in_rae_in_mcg, zn_in_mg) %>% 
   add_row(
      ref_foodid = "103",
      ref_fooditem =  "Barley (Incl. Beso: roasted & milled barely)",
      original_food_id = "170284",
      original_food_name = "Barley, pearled, raw",
      fct_name = "USDA", 
      moisture_in_g = 10.1,
      energy_in_kcal = 352,
      vitamina_in_rae_in_mcg = 1, 
      zn_in_mg = 2.13) %>% 
   add_row(
      ref_foodid = "301",
      ref_fooditem =  "Niger Seed",
      original_food_id = "H015",
      original_food_name = "Niger seeds, black (Guizotia abyssinica)",
      fct_name = "INFCT", 
      moisture_in_g = 4.6,
      energy_in_kcal = 2144/4.1868, #calculated
      vitamina_in_rae_in_mcg = 2.15/12+295/24, #calculated 
      zn_in_mg = 4.98) %>%  #maybe change to biblio, seems high...
   add_row(
      ref_foodid = "302",
      ref_fooditem =  "Linseed",
      original_food_id = "H014",
      original_food_name = "Linseeds (Linum usitatissimum)",
      fct_name = "INFCT", 
      moisture_in_g = 5.48,
      energy_in_kcal = 1857/4.1868, #calculated
      vitamina_in_rae_in_mcg = 1.05/12+ 92/24, #calculated 
      zn_in_mg = 4.86) %>% 
   add_row(
      ref_foodid = "602",
      ref_fooditem =  "Kocho",
      original_food_id = "biblio1",
      original_food_name = "Kocho (multiple varietes)",
      fct_name = "Bosha et al. 2016", 
      moisture_in_g = 100-37.18, #calculated
      energy_in_kcal = 400/4.1868, #calculated
      vitamina_in_rae_in_mcg = NA,  
      zn_in_mg = 2.63*(100-62.82)/100) %>% #calculated
   add_row(
      ref_foodid = "603",
      ref_fooditem =  "Bulla",
      original_food_id = "biblio2",
      original_food_name = "Bulla",
      fct_name = "Daba, T. and Shigeta, M., 2016", 
      moisture_in_g = 7.8, 
      energy_in_kcal = 363, 
      vitamina_in_rae_in_mcg = NA,  
      zn_in_mg = 0.20)  %>%
   add_row(
      ref_foodid = "901",
      ref_fooditem =  "purchased Injera",
      original_food_id = "biblio3",
      original_food_name = "Injera: from fermented white and red teff dough",
      fct_name = "Abebe, et al.,  2007", 
      moisture_in_g = 67.8, 
      energy_in_kcal = NA , 
      vitamina_in_rae_in_mcg = 0,  
      zn_in_mg = 0.93)  %>%
      add_row(
      ref_foodid = "210",
      ref_fooditem =  "Processed pulses (Shiro)",
      original_food_id = "174288",
      original_food_name = "Chickpea flour (besan)",
      fct_name = "USDA", 
      moisture_in_g = 10.3,
      energy_in_kcal = 387,
      vitamina_in_rae_in_mcg = 2, 
      zn_in_mg = 2.81)
   


ess4_nv <- ess4_ken %>% bind_rows(., missig_values) %>% 
   group_by(ref_foodid) %>% 
   summarise_if(is.numeric, mean, na.rm = T)
   


ess4_ref <- ess4_ken %>% bind_rows(., missig_values) %>% 
   group_by(ref_foodid) %>% 
   summarise_if(is.character, ~paste(., collapse = ", "))

ess4_fct <- ess4_ref %>% left_join(., ess4_nv) %>% glimpse()

#Function to assign ref. per NV (when all coming from the same ref.)

nv_name <- c("moisture", "energy", "vita_rae", "zn") 

for(i in nv_name){
   
ess4_fct[c(paste0("original_food_id_",i), paste0("original_food_name_", i),
 paste0("fct_name_", i))] <- ess4_fct[c("original_food_id", "original_food_name", "fct_name")]
}

ess4_fct %>% filter(is.na(zn_in_mg))

ess4_fct$energy_in_kcal[ess4_fct$ref_foodid == "901"] <- 197*(100-67.8)/(100-48.8)
ess4_fct$original_food_id_energy[ess4_fct$ref_foodid == "901"] <- "01_186"
ess4_fct$original_food_name_energy[ess4_fct$ref_foodid == "901"]  <- "Teff, whole grains, boiled* (as part of a recipe)"
ess4_fct$fct_name_energy[ess4_fct$ref_foodid == "901"]  <- "WAFCT"

ess4_fct$vitamina_in_rae_in_mcg[ess4_fct$ref_foodid %in% c("602", "603")] <- c(63*(100-62.82)/(100-62.4), 63*(100-7.8)/(100-62.4))
ess4_fct$original_food_id_vita_rae[ess4_fct$ref_foodid %in% c("602", "603")] <- "02_042"
ess4_fct$original_food_name_vita_rae[ess4_fct$ref_foodid %in% c("602", "603")] <- "Plantain, ripe, ivory flesh, raw"
ess4_fct$fct_name_vita_rae[ess4_fct$ref_foodid %in% c("602", "603")] <- "WAFCT"
   
ess4_fct <- ess4_fct %>% 
   relocate(zn_in_mg, .before = original_food_id_zn) %>% 
   relocate(vitamina_in_rae_in_mcg, .before = original_food_id_vita_rae) %>% 
   relocate(energy_in_kcal, .before = original_food_id_energy) %>% 
   relocate(moisture_in_g, .before = original_food_id_moisture) %>% 
   glimpse()

ess4_fct$vitamina_in_rae_in_mcg <- as.numeric(ess4_fct$vitamina_in_rae_in_mcg)

hist(ess4_fct$vitamina_in_rae_in_mcg)

ess4_fct %>% write.csv(here::here("output", "ess4-fct.csv"))
