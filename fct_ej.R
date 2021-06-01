
library(tidyverse)
library(fuzzyjoin)

####================== LOADING DATASET =============================####


fct.t <- read.csv(here::here('Simplified-match-FBS-region_2021-04-30.csv')) 

dictionary <- read.csv(here::here('MAPS_Dictionary_v2.2.csv'))

#Edward regional FCT as per his Suppl. mat. table 2

fct <- readxl::read_xlsx(here::here("data", 'ppl12144-sup-0002-tables2.xlsx'), 
                         sheet = "S Table 2", skip = 2) %>% select(1:8) %>% 
  janitor::clean_names()

### ---- fct-ej-spread

#We have spread Edward FCT (to get same format as other FCT), we have also 
#kept the source of the data, with the name, ref of each nutrient (as 
#same order as in the table)

fct.s <- fct %>% 
  rename(quantity = "concentration_100_g_1_fresh_weight", 
         food_item = "item_fbs_category") %>% 
  unite("compo", c("nutrient", "unit"), sep = "_", remove = F) %>% 
  unite("source", c("fitted_item_as_decribed_in_published_source", 
                    "nutrient", "source"), sep = "_") %>% 
  select(-c(item, source, unit)) %>% 
  pivot_wider(names_from = compo, 
              values_from = quantity) %>%
  janitor::clean_names()

#
#group_by(food_item, region) %>% 
#
#  summarise(source = list(source), 
#
#            across(Energy_kcal:Zn_mg, mean, na.rm = TRUE)) %>% 


#We are removing the references/source data and join it with the authors/ publi
#info in the original file

#we are saving the data "Alone"

fct.s %>% 
write.csv(here::here("data", "EJ-regional-SSA-spread-FCT.csv"), row.names = F)

#We are creating a identical structure but with the citation/referencing data.

fct.ref <- fct %>% 
  rename(quantity = "concentration_100_g_1_fresh_weight", 
         food_item = "item_fbs_category") %>% 
  unite("source", c("fitted_item_as_decribed_in_published_source", 
                      "source"), sep = "_") %>% 
  select(-c(item, unit, quantity)) %>% 
  pivot_wider(names_from = nutrient, 
              values_from = source) %>% 
  janitor::clean_names() 

fct.ref %>% 
  write.csv(here::here("metadata", "EJ-regional-SSA-spread-citation.csv"),
            row.names = F)

#### ---- fct-ej-to-genus-code

#Attaching genus code_3 and name to each food item in the regional fct
#need to be reviewed

fct.t <- fct.t %>% select(Region:foodname_3, -Nutrient) %>% 
  mutate(Item.FBS.category = str_replace(Item.FBS.category, "Rice" ,
                                         "Rice (Milled Equivalent)"))
  

fct <- fct %>% 
      left_join(., fct.t,
        by = c("food_item" = "Item.FBS.category", "Region")) %>%
  rename(
  FE2_3 = "code_3", 
  FoodName_3 = "foodname_3") %>% 
  mutate(FoodName_3 = str_replace_all(FoodName_3, #solving a mismatch between dictionary (new version)
    c("eggs, chicken" = "egg, chicken, raw",      #and fct-food names (old version)
    "milk, cow" = "milk, cow, whole, raw",
    "pig meat, fresh, raw" = "pig meat, without bones, fresh, raw", 
    "beef, fresh, raw" = "beef, without bones, fresh, raw"))) %>% 
     mutate_at("FoodName_3", str_to_lower) %>% glimpse()

#THIS IS NOT WORKING, I NEED TO FIX IT!! 
#but not today....
  
#  mutate(FE2_3 = str_replace_all(FE2_3,
 #            c("[[A031F#F02.A06BR$F01.A04SF$F27.A031F]]" = "A031G#F28.A07HS",
   #            "A02LV#F02.A06AZ\\$F01.A057E$F27.A02LV" = "A02LY#F28.A07HS",
  #             "A01RG#F28\\.A07HS" = "A01RG#F28\\.A07HS$F20\\.A07QM",
  #            "A01QX#F28\\.A07HS" = "A01QX#F28\\.A07HS\\$F20\\.A07QM" ))) %>% #beef
#   mutate_at("FoodName_3", str_to_lower) %>% glimpse()

#filter "blanks" and NA 

fct %>% filter(!str_detect(FE2_3, "") | is.na(FE2_3)) %>% pull(food_item)

fct %>% filter(str_detect(FE2_3, "") | !is.na(FE2_3)) %>% pull(FE2_3)

fct %>% filter(!food_item %in% c("Aquatic Animals, Others", 
                                 "Oilcrops Oil, Other", 
                                 "Ricebran Oil")) %>% pull(FoodName_3)

#### ---- fct-ej.genus-code-to-genus-groups

### ---- Attaching all genus list 

#fct.genus <- fct %>% filter(str_detect(FE2_3, "")) %>%
  #left_join(., dictionary, by = c("FE2_3", "FoodName_3")) %>% glimpse()

#THIS IS A LAME WORK AROUND BUT I CAN'T DO ANYTHING ELSE TODAY!!

fct.genus <- fct %>% filter(str_detect(FE2_3, "")) %>%
  left_join(., dictionary, by = "FoodName_3") %>% glimpse()

#fct.genus <- fct %>% filter(!food_item %in% c("Aquatic Animals, Others", 
 #                                             "Oilcrops Oil, Other", 
  #                                            "Ricebran Oil")) %>%
  #left_join(., dictionary)

#to avoid conflict we need to filter out those items without FE2_3
#until are fixed


#CHECKS BEFORE SAVING!!

#we have joined by FoodName_3 --> But we are going to use ID_1 and FoodName_1 
#to join FAO - FOOD BALANCE SHEET!! Check if there is consistency!!


fct.genus %>% filter(str_detect(FoodName_3, "egg")) %>% pull(ID_1, FoodName_1)

dictionary %>% filter(str_detect(FoodName_3, "milk")) %>% pull(FE2_3, FoodName_3)

#we are removing the "source" variable to make the file lighter

fct.genus %>% 
  select(-source) %>% 
  write.csv(here::here("regional-SSA-fct_2021-05-04.csv"), row.names = F)

#Loading the fct to "create" a middle region fct data copied from west
#Africa, this should be reviewed in the near future

fct.regional <- read.csv(here::here("regional-SSA-fct_2021-05-04.csv"))

fct.SSA <- fct.regional %>% filter(Region == "W") %>%
  mutate(Region = "M") %>% bind_rows(., fct.regional) 

write.csv(fct.SSA, here::here("regional-SSA-fct_v.1.3.csv"), row.names = F)

fct.regional <- read.csv(here::here("MAPS_regional-SSA-fct_v1.4.csv"))

splitregion <- fct.regional %>% 
  group_by(region) %>% 
  group_split()

allNames <- fct.regional %>% 
  group_by(region) %>% 
  group_keys()


  for(i in 1:4){
  saveName = paste0("MAPS_", i, "-Africa_v1.x.csv")
  write.csv(splitregion[[i]], file = saveName)
  }

#KEEP working on this script