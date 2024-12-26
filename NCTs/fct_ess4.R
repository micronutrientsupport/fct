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



# 0) Loading previous survey matches

source("fct_ess3.R")

#New dataset from 2018-2019.

ess4_food_list <- read.csv(here::here("inter-output",
                                      "eth_fct_match_v1.csv")) %>% 
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


ess4_food_list <- ess4_food_list %>% left_join(., ess4_food) %>% 
  filter(!is.na(ID_3)) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   rename(ref_foodid_ess3 = "ref_foodid", 
          ref_foodid = "fcode") %>% 
   mutate_at("ref_foodid", as.character) %>% 
   bind_rows(., ess4_food_st)


# TODO: Updted to do the matching using fct_dict

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
          energy_in_kcal,vitamina_in_rae_in_mcg, zn_in_mg) # Selecting nutrient from KE18

dictionary.df %>% filter(ID_3 == "1441.01")

#WAFCT, 2019

source("wafct.R")

missig_values %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_wafct, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(energy_in_kcal)) %>% 
   select(1:3)

missig_values <- missig_values %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_wafct, by = c("ID_3" = "food_genus_id")) %>% 
   filter(!is.na(energy_in_kcal)) %>% 
   select(1:3, original_food_id, original_food_name, fct_name, moisture_in_g,
          energy_in_kcal,vitamina_in_rae_in_mcg, zn_in_mg) %>% 
   add_row(
      ref_foodid = "103",
      ref_fooditem =  "Barley (Incl. Beso: roasted & milled barely)",
      ID_3 = "115.01", 
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
      ID_3 = "1449.9.01",
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
      ID_3 = "1441.01",
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
      ID_3 = "23170.02.01", 
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
      ID_3 = "23170.02.02", 
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
      ID_3 = "23140.08.01", 
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

#Loop to assign ref. per NV (when all coming from the same ref.)

nv_name <- c("moisture", "energy", "vita_rae", "zn") 

for(i in nv_name){
   
ess4_fct[c(paste0("original_food_id_",i), paste0("original_food_name_", i),
 paste0("fct_name_", i))] <- ess4_fct[c("original_food_id", "original_food_name", "fct_name")]
}

# Missing values for Zn

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

#ess4_fct %>% write.csv(here::here("output", "ess4-fct.csv"), row.names = F)


## UPDATE matches (26/06/2024) -----
# 610	Other tuber or stem (SPECIFY)/jinjibl  1657.01  	13021

# Loading ess4 NCT (v1.0.0)
nct <- read.csv(here::here("output", "ess4-fct.csv"))
# Converting variable into character
nct$ref_foodid <- as.character(nct$ref_foodid)

# Loading the dictionary & fct library
source(here::here("MAPS_Dictionary-Protocol.R"))
source(here::here("MAPS_fct_load.R"))

# Loading ess4 cleaned
data.df <- read.csv(here::here( "data", "hces", 
                                "food-cons_ess4_v1.0.0.2.csv"))
names(data.df)

# Converting variable into character
data.df$original_food_id <- as.character(data.df$original_food_id)

#cassa
dictionary.df %>% filter(ID_3 == "1699.03")
dictionary.df %>% filter(ID_2 == "23170.02") %>% View()
dictionary.df %>% filter(grepl("inje", FoodName_3)# &
                          # grepl("pulses", FoodName_1, ignore.case = TRUE)
                        )
fct_cover %>% filter( 
  #source_fct == "KE18" , 
#  grepl("ense", scientific_name, ignore.case = TRUE)
  grepl("beet", food_desc, ignore.case = TRUE) #&
 # grepl("VEGETABLE", food_group, ignore.case = TRUE)
  ) %>%  View()

fct_dict %>% filter(!is.na(ID_3)) %>% 
  filter(
  #  grepl("niger", food_desc, ignore.case = TRUE)
    grepl("vicia", scientific_name, ignore.case = TRUE)) %>% 
  select(fdc_id, source_fct, food_desc, ID_3) %>% View()

data.df %>% distinct(original_food_id, original_food_name) %>% 
  left_join(., nct, by = c("original_food_id" = "ref_foodid")) %>% 
  filter(is.na(ID_3))

food_list <-  data.df %>% distinct(original_food_id, original_food_name) %>% 
  left_join(., nct, by = c("original_food_id" = "ref_foodid")) %>% 
  distinct(original_food_id, original_food_name.x, ID_3)

# Final review w/ S.M. matches:

# NEW:
# 210	Processed pulses (Shiro)  23170.03.01
# 713	Other condiments/yeshro kimemoch	1699.03	13028 KE18
# 904	Other purchased prepared food/Buna	23912.02.01	12003	Coffee, instant, dry powder or granules
# 904	Other purchased prepared food/Tbis	21116.03	7016	Goat, medium fat, raw
# 904	Other purchased prepared food/Beer	24310.01.01 168746	beer, regular

# Adding missing dict. codes
food_list$ID_3[food_list$original_food_id == "210"] <- "23170.03.01"
food_list$ID_3[food_list$original_food_id == "713"] <- "1699.03"
food_list$ID_3[food_list$original_food_id == "904"] <- "23912.02.01,21116.03,24310.01.01"

# Mismatch btween dict code and dictionary entry - need update
# 407 Moringa/Shiferaw/Halloka 1699.08    
# 901 Injera (purchased)       23140.08.01 --> F1232.33
# 606 Cassava                  01520.01.01

# Updated codes
food_list$ID_3[food_list$original_food_id == "407"] <- "1290.9.18"
food_list$ID_3[food_list$original_food_id == "901"] <- "F1232.33"
food_list$ID_3[food_list$original_food_id == "606"] <- "1520.01.01"
food_list$ID_3[food_list$original_food_id == "503"] <- "1316.04,1316.05"
food_list$ID_3[food_list$original_food_id == "609"] <- "1801.01"

# Mismatch btween dict code and fct library - need update (see docu for more info)
# Updated codes
food_list$ID_3[food_list$original_food_id == "701"] <- "21116.02,21116.03,21115.01,21115.02"
food_list$ID_3[food_list$original_food_id == "106"] <- "118.02,118.03"
food_list$ID_3[food_list$original_food_id == "201"] <- "1702.02"
food_list$ID_3[food_list$original_food_id == "207"] <- "1702.02"


food_list %>% filter(is.na(ID_3))

# Separte multi-matches and check 
food_list <- food_list %>% separate_rows(ID_3) 

## a) dictionary codes
food_list %>% filter(!is.na(ID_3)) %>% 
          left_join(., dictionary.df) %>% 
  filter(is.na(FoodName_3)) %>% select(original_food_id, original_food_name.x, ID_3)

## b) data availability

 food_list %>% filter(!is.na(ID_3)) %>% 
  left_join(., fct_dict) %>% 
  filter(is.na(fdc_id)) %>% 
  select(original_food_id, original_food_name.x, ID_3) %>% 
   View()

## c) fixing missing values

 food_list <-  food_list %>% 
   add_count(original_food_id) %>% 
   mutate(
     wt = 1/n, 
     confidence = "m") %>% 
   select(original_food_id, original_food_name.x, ID_3, confidence, wt)  

 # Changes in the confidence 
food_list$confidence[food_list$original_food_id == "210"] <- "l"
food_list$confidence[food_list$original_food_id == "904"] <- "l"
food_list$confidence[food_list$original_food_id == "704"] <- "l"

food_list$confidence[food_list$original_food_id == "208"] <- "h"
food_list$confidence[food_list$original_food_id == "502"] <- "h"
food_list$confidence[food_list$original_food_id == "608"] <- "h"
food_list$confidence[food_list$original_food_id == "302"] <- "h"
food_list$confidence[food_list$original_food_id == "202"] <- "h"
food_list$confidence[food_list$original_food_id == "303"] <- "h"
food_list$confidence[food_list$original_food_id == "406"] <- "h"
food_list$confidence[food_list$original_food_id == "601"] <- "h"
 
food_list %>%  left_join(., dictionary.df %>% select(ID_3, FoodName_3) %>%
                           filter(!is.na(ID_3))) %>% 
   arrange(wt)  %>% View()
 
## HCES standard names

standard_names <- c("household_id", "latitud", "longitud", "urbanity", 
                    "wealth_quintile", "household_expenditure",
                    "interview_date",   "original_food_id",
                    "original_food_name",
                    "food_genus_id", "food_genus_confidence", 
                    "amount_consumed_in_g")

# Getting final MAPS output
hces_cons <-  data.df %>% left_join(., food_list %>% 
                         dplyr::rename( food_genus_id = "ID_3",
                                        food_genus_confidence = "confidence"), 
                       relationship = "many-to-many") %>% 
   # Adjusting the consumption to the weight (multi-matched items)
   mutate(amount_consumed_in_g = amount_consumed_in_g*wt) %>% 
   select(all_of(standard_names)) 
 
file_name <- paste0("MAPS_", hces, "_food-cons_")

write.csv(hces_cons,
          here::here("output",
                     paste0(file_name, "v1.0.2.csv")), 
          row.names = FALSE)
