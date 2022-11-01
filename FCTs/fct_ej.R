

####================== LOADING  =============================####

# Libraries
library(tidyverse)
library(fuzzyjoin)

# Dictionary created and updated for the project
#contains the genus code
source("MAPS_Dictionary-Protocol.R")

#Data
#Joy et al. regional FCT as per his Suppl. mat. table 2

fct <- readxl::read_xlsx(here::here("data", 'ppl12144-sup-0002-tables2.xlsx'), 
                         sheet = "S Table 2", skip = 2) %>% select(1:8) %>% 
  janitor::clean_names()



#Data on the fbs-fct from Joy et al. that was coded *manually* with
#the genus code

fct.t <- read.csv(here::here('metadata',
                             'Simplified-match-FBS-region_v1.6.csv')) 


######-----------------------VARIABLE STANDARDIZATION---------------------########

#Variable names for data standardization
variables <- read.csv(here::here("metadata", "fct-variable-names.csv"))

#getting the names of all the standard variables names, to filter them afterward
var.name <- variables %>% select(Column.Name) %>% pull

#getting all the MAPS-standard variables included in the dataset.

var.dat <- variables %>% spread(Column.Name, Description) %>% 
  mutate_all(as.numeric) %>%                               #fixing the type of
  mutate_at(c("original_food_id", "original_food_name",
              "data_reference_original_id","food_genus_id", 
              "food_genus_description", "food_genus_confidence",         
              "food_group","food_subgroup" , "fct_name"),   #variables so I can
            as.character) 

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
  mutate(Item.FBS.category = ifelse(Item.FBS.category == "Rice" ,
               "Rice (Milled Equivalent)", Item.FBS.category )) %>% 
  rename(region = "Region") 
  
fct <- fct.s %>% 
      left_join(., fct.t,
        by = c("food_item" = "Item.FBS.category", "region")) %>%
  rename(
  FE2_3 = "code_3", 
  FoodName_3 = "foodname_3") %>% 
  mutate(FoodName_3 = str_replace_all(FoodName_3, #solving a mismatch between dictionary (new version)
    c("eggs, chicken" = "egg, chicken, raw",      #and fct-food names (old version)
    "milk, cow" = "milk, cow, whole, raw",
    "pig meat, fresh, raw" = "pig meat, without bones, fresh, raw", 
    "beef, fresh, raw" = "beef, without bones, fresh, raw"))) %>% 
     mutate_at("FoodName_3", str_to_lower)

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

fct.genus <- fct %>% 
  left_join(., dictionary.df, by = c("FoodName_3")) %>% glimpse()


fct.genus %>% filter(is.na(ID_1)) 

#CHECKS BEFORE SAVING!!

#we have joined by FoodName_3 --> But we are going to use ID_1 and FoodName_1 
#to join FAO - FOOD BALANCE SHEET!! Check if there is consistency!!


fct.genus %>% filter(str_detect(FoodName_3, "egg")) %>%
  pull(ID_1, FoodName_1)

fct.genus %>% filter(str_detect(FoodName_3, "egg")) %>%
  pull(ID_3, FoodName_1)

dictionary.df %>% filter(str_detect(FoodName_3, "milk")) %>% 
  pull(FE2_3, FoodName_3)

#saving the original regional-fct with genus codes
#save into the ~FoodDictionary/data
#
#fct.genus %>% 
#  write.csv(here::here("output",
#    "MAPS_three-regions-Africa-fct_v1.6.csv"), row.names = F)
#
#

#fct.genus <-  read.csv(here::here("data",
 #                         "MAPS_regional-SSA-fct_v1.6.csv"))

#Loading the fct to "create" a middle region fct data copied from west
#Africa, this should be reviewed in the near future.
#And adding MAPS format to the dataset and confidence of the match

fct.confidence <- read.csv(here::here(
  "MAPS_regional-SSA-fct_v1.4.csv")) %>% select(food_genus_id, 
                                                food_genus_confidence, 
                                                region)


#There are some extra-elements due to the merging fct.confidence
#that I am unable to identify. 

fct.genus <- fct.genus %>% filter(region == "W") %>%
  mutate(region = "M") %>% bind_rows(., fct.genus) %>% 
  mutate(fct_name = "regional-SSA-fct") %>% 
   rename(
    original_food_name = "food_item",
    energy_in_kcal = "energy_kcal",
    totalprotein_in_g = "protein_g",
    totalfats_in_g = "fat_g",
    carbohydrates_in_g = "carbohydrates_available_g", 
    fibre_in_g = "fibre_g", 
    ca_in_mg = "ca_mg", 
    fe_in_mg = "fe_mg",
    mg_in_mg = "mg_mg",
    zn_in_mg = "zn_mg", 
    cu_in_mg = "cu_mg",
    i_in_mcg = "i_mg",
    se_in_mcg = "se_mg",
    phytate_in_mg = "phytate_mg",
    food_genus_id = "ID_3",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1") %>% 
   left_join(., fct.confidence, by = c("food_genus_id", "region")) %>%
  left_join(., var.dat) %>% select(var.name, region) 

#checking that all food items has its confidence match

fct.genus %>% filter(is.na(food_genus_confidence)) %>% 
  distinct(food_genus_id, food_genus_description)

#missing three items

fct.genus <- fct.genus %>% mutate(food_genus_confidence = case_when(
  food_genus_id == "1587.01" ~ "l" ,   #aquatic animals = frog
  food_genus_id == "1522.01" ~ "l" , #fish body oil = cod, body, oil
  food_genus_id == "34550.01" ~ "l" , #oil crop others = vegetable oil
  food_genus_id == "21691.01.01"  ~ "h", #ricebran oil = rice bran oil
  food_genus_id == "1701.02"  ~ "m ", #common beans = beans and products
  TRUE ~ food_genus_confidence))

fct.genus$food_group[fct.genus$food_genus_id == "23511.02.01"] <- "Other foods"

fct.genus %>% distinct( food_genus_id, original_food_name, region) %>% 
  filter(food_genus_id == "1323.01")

fct.genus %>% filter(food_genus_confidence != "l") %>% 
  filter(food_genus_id == "1323.01")

#fct.genus %>% filter(filter %in% c(food_genus_id == "1323.01" & food_genus_confidence != "l"))

names(fct.genus)
#Saving four independent regional FCT
#KEEP working on this script


read.csv(here::here("output", "MAPS_Western-Africa_v1.9.csv")) %>% 
  filter(is.na(food_group))

#need to save them!!

splitregion <- fct.genus %>% 
  group_by(region) %>% 
  group_split()

allNames <- c("Eastern", "Middle", "Southern", "Western")



      for(i in 1:4){
  saveName = paste0("output/MAPS_", allNames[i] , "-Africa_v2.0.csv")
  readr::write_excel_csv(splitregion[[i]], file = saveName)
  }



read.csv(here::here("metadata", "EJ-regional-SSA-spread-citation.csv")) %>% 
  filter(str_detect(food_item, "Cassava"), region == "W")

#Piece of code that check that FBS and regional FCT matches. 

read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv")) %>%
  select(food_genus_id) %>% unique() %>% filter(!is.na(food_genus_id)) %>%
  inner_join(.,read.csv(here::here("data", "MAPS_Western-Africa_v1.6.csv")))


read.csv(here::here("data", "MAPS_Western-Africa_v1.6.csv")) %>%
  anti_join(., read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv")) %>%
              select(food_genus_id) %>% unique() %>% filter(!is.na(food_genus_id)))

read.csv(here::here("output", "MAPS_Western-Africa_v1.8.csv")) %>%
  anti_join(., fbs, by = c("food_genus_id")) %>%
              select(food_genus_id) %>% unique() %>% filter(!is.na(food_genus_id))
