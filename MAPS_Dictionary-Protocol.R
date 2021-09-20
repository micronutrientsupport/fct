
#install.packages("fuzzyjoin")

library(tidyverse)
library(fuzzyjoin)


####================== LOADING DATASET =============================####
# 
# 
#  fbs <- read.csv(here::here('data',
#                            'Simplified-match-FBS-region_v1.5.csv')) 
# 
# fct <- readxl::read_xlsx(here::here("data", 'Food.item.dictionary.MAPS.v0.3.xlsx'), 
#                          sheet = "S Table 2")
# 
# fbs2013 <- read.csv(here::here( 'data', 'FBS_africa-region_2013_2021-02-11.csv'))
# 
# dictionary <- read.csv(here::here('MAPS_Dictionary_03.csv'))
# 
# fgroups <- read.csv(here::here('EJ_Foodgroup.csv')) %>% select(1:2)
# 
# 
# 
##----- creating the food-dictionary-----

#Updating MAPS-Dictionary with EJ food groups

#dataset with the food groups and names
#group <- readr::read_csv("FoodName_0, ID_0
#Animal Products,	AP
#Cereals,	CE
#Fruits and Vegetables,	FV
#Pulses and Beans,	PB
#Roots and Tubers,	RT
#Other foods,	OT")
#
##Using fuzzy join to join column due to slight differences in names
## e.g., removed "and products" in fgroups dataset
#
#dict_group <- dictionary %>%
#  regex_left_join(fgroups, by = c(FoodName_1 = "Item.FBS"))
#
#dict_group <- dict_group %>%
#  left_join(fgroups, by = c(FoodName_1 = "Item.FBS"))
#
#dict_group <- dict_group %>% 
#  mutate(Foodgroup = ifelse(!is.na(Foodgroup.y),
#                            Foodgroup.y, Foodgroup.x))
#
##Select and rename variables of interest
#dict_group <- dict_group %>% 
#  select(-c(ID_0, FoodName_0)) %>% 
#  rename(ID_0 = "Foodgroup") %>% 
#  select(-c(starts_with("Foodgroup")))
#
##joining names and group code
#dict_group <- left_join(dict_group, group)

##reordering variables
#dict_group <- dict_group %>% 
#  relocate(c("ID_0", "FoodName_0"), .before = ID_1) %>% 
#  mutate_at("FoodName_3", str_to_lower)
#
##saving csv
#dict_group %>%
#  write.csv(here::here("MAPS_Dictionary_v1.0.csv"))
#
#### --------------- Creating a template for data collection-----
#
##fbs <- fbs %>% filter(!is.na(Item)) %>% select(!starts_with("X"))
#

#write.csv(fbs, here::here('Simplified-match-FBS-region_2021-02-10.csv'))

#### --------------- Creating a template for code and name tags----
#
#fbs_tag <- fbs %>% 
#  rename(foodname_fbs = 'Item.FBS.category') %>% 
#  select( 'foodname_fbs', 'code_3', 'foodname_3', 'Region') %>%
#  filter(str_detect(code_3, "[^[:alnum:] ]"))
#
#
#fct1 <- left_join(fct, fbs_tag,
#                  by = c("Item (FBS category)" = "foodname_fbs"))
#
#fct1 <- fct1 %>% distinct(Item, .keep_all = TRUE)
#
##fct2 <- fct1 %>% group_split(Region)
#
##DONT RUN THIS!-----
#x <- fct1 %>% group_by(Region) %>% 
#  select(Region, 
#         Nutrient, 
#         `Fitted item (as decribed in published source)`:foodname_3.y) %>% 
#  unite("Nutrient", c("Nutrient", "Unit"), sep = "_") %>% 
#  pivot_wider(names_from = Nutrient, 
#              values_from = `Concentration (100 g-1 fresh weight)`) %>%
#  group_split(Region)
#
########################
#
#x <- fct1 %>% 
#  rename(code03 = 'code_3.y', 
#         foodname03 = "foodname_3.y", 
#         foodname_originalfct = "Fitted item (as decribed in published source)") %>% 
#  select(Region, 
#         Nutrient, 
#         foodname_originalfct:foodname03) %>% 
#  unite("Nutrient", c("Nutrient", "Unit"), sep = "_") %>% 
#  pivot_wider(names_from = Nutrient, 
#              values_from = `Concentration (100 g-1 fresh weight)`, 
#              values_fn = mean)  #when multiple values were collated 
##in a list give the mean
#
## x$foodname03 <- str_to_lower(x$foodname03)
## 
## x <- left_join(x, dict_group, by = c("code03" = "FE2_3",
##                                      "foodname03" = "FoodName_3"))
## 
# x %>% filter(!is.na(code03)) %>% 
#   write.csv(here::here("regional-fct_2021-02-22.csv"))
# 
# 

####-----New GENuS code --------#####


#Fixing names and/ typos
# 
# dictionary <- dictionary %>% mutate(FoodName_3 = case_when(
#   ID_3 == "22211.01" ~ "milk, cow, full-fat, powder, unfortified", 
#   ID_3 == "24490.02" ~ "sugar sweetened beverage, cola",
#   TRUE ~ FoodName_3))
#

#Adding new genus to the dictionary for infant formulae
#and changing to lower case variable FoodName_1

#dictionary <- dictionary %>% add_row(
#   ID_0 = "OT",
#   FoodName_0 = "Other foods", 
#   ID_1 = 2680,
#   FoodName_1 = "Infant food and products", 
#   ID_2 = "23991.01", 
#   FoodName_2 =  "infant food",
#   ID_3 = "23991.01.02",
#   FE2_3 = "A03QA#F04.A02PR",
#   FoodName_3 = "infant formula, milk-based, casein, powder") %>% 
#   mutate_at("FoodName_1", str_to_lower)


# dictionary <- dictionary %>% add_row(
#   ID_0 = "FV",
#   FoodName_0 = "Fruits and Vegetables", 
#   ID_1 = 2617,
#   FoodName_1 = "apples and products", 
#   ID_2 = "21435.01", 
#   FoodName_2 =  "apple juice",
#   ID_3 = "21435.01.01",
#   FE2_3 = "A039M#F08.A032J",
#   FoodName_3 = "apple, juice, sweetened") 

#write dictionary with new genus

#write.csv(dictionary, here::here("MAPS_Dictionary_v2.3.csv"))
########===========END=============##### 

#save as output a list of genus_id and genus_name available

dictionary.df <- read.csv(here::here("metadata", "MAPS_Dictionary_v2.5.csv")) %>% 
  select(-starts_with("X"))

#It was a typo that duplicated two contiguous food items,
#here we are solving it

dictionary.df$ID_3[dictionary.df$FoodName_3 == "cake, banana"] <- "F0022.07"

#Adding a new item for ihs5
#mucuna == 837 == 1701.4 == velvet bean, dried, raw 

dictionary.df <- dictionary.df %>% add_row(
   ID_0 = "PB",
   FoodName_0 = "Pulses and Beans", 
   ID_1 = 2546,
   FoodName_1 = "beans and products", 
   ID_2 = "1701", 
   FoodName_2 =  "beans, dry",
   ID_3 = "1701.04",
   FE2_3 = NA,
   FoodName_3 = "velvet bean, dried, raw") 
 
#dictionary %>% 
#write.csv(here::here('MAPS_Dictionary_v2.5.csv'), row.names = F)

#dictionary %>% select(ID_3, FoodName_3) %>% filter(str_detect(ID_3, "\\b")) %>% 
 # rename(food_genus_id = "ID_3",
  #       food_genus_name = "FoodName_3") %>% 
  # write.csv(here::here('output', 'MAPS_Dictionary_v2.5.csv'), row.names = F)


#---tracking-issue-2

#checking ids duplication at group level 2 (ID_2):
dictionary.df %>% distinct(FoodName_2, ID_2) %>%
  count(ID_2) %>% arrange(desc(n))

#checking what items with duplicated ids
dictionary.df %>% filter(ID_2 %in% c("1510", "1530"))

#checking that the new proposed ids
dictionary.df %>% filter(ID_2 %in% c("2510", "2530"))

#changing fish ids to new ones:

dictionary.df <-  dictionary.df %>% mutate(ID_2 = case_when(
  FoodName_2 == "freshwater fish, liver oil" ~ "2510",
  FoodName_2 == "pelagic fish, frozen, fillet" ~ "2530", 
  TRUE ~ ID_2
))

#New codes coming from FBS and FBSH

#FBS
#2558 == 1442.01 (mustard seed, dried, raw) New
# 2659 == 24110.01 (alcohol, 80perc) New 
#FBSH
#2562 == 1491.02.01 (palm kernel, raw) New

dictionary.df <- dictionary.df %>% add_row(
  ID_0 = "OT",
  FoodName_0 = "Other foods", 
  ID_1 = 2558,
  FoodName_1 = "rape and mustardseed and products", 
  ID_2 = "1442", 
  FoodName_2 =  "mustard seed",
  ID_3 = "1442.01",
  FE2_3 = "A015S#F28.A07KG$F28.A07HS",
  FoodName_3 = "mustard seed, dried, raw") %>% 
  add_row(
    ID_0 = "OT",
    FoodName_0 = "Other foods", 
    ID_1 = 2659,
    FoodName_1 = "alcohol, non-food and products", 
    ID_2 = "24110", 
    FoodName_2 =  "undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher",
    ID_3 = "24110.01",
    FE2_3 = NA,
    FoodName_3 = "alcohol, 80perc") %>% 
    add_row(
      ID_0 = "OT",
      FoodName_0 = "Other foods", 
      ID_1 = 2562,
      FoodName_1 = "palm kernels and products", 
      ID_2 = "1491.02", 
      FoodName_2 = "palm kernels [only calories]",
      ID_3 = "1491.02.01",
      FE2_3 = "A0DAJ#F28.A07HS",
      FoodName_3 = " palm kernel, raw") 

#making a distinction between two rice that were named the same
#we will use local rice whenever possible 
#default rice white == "23161.02.01"

dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.01.01"] <- "rice grain, imported, white, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.02.01"] <- "rice grain, local, white, raw"



#Run this to over-write any new upgrades in adding new food dictionary codes
#in dictionary folder


#IMPORTANT: to keep all folder updated run this code!!
#Saving Food-Dictionary into all r-project that use it

#here we are checking where the previous version was stored:
#
#rproject.path <- "C:/Users/LuciaSegoviaDeLaRevi/OneDrive - London School of Hygiene and Tropical Medicine/MAPS/02_working-files/r-project"
#
##knowing all the MAPS_Dictionary-Protocol.R
#x  <- list.files(rproject.path, 
#           pattern = "MAPS_Dictionary-Protocol.R",
#           recursive = TRUE)
#
#
#
## find the files that you want
#dictionary.files <- list.files(rproject.path, 
#                               pattern = "MAPS_Dictionary_v2.5.csv",
#                               recursive = TRUE) 
#
## copy the files to the new folder
#file.copy("MAPS_Dictionary-Protocol.R", file.path(rproject.path,
#                                                  x[2]), overwrite = TRUE)
#
##a loop to check that paths are created
#for(i in 1:length(x)){
#  
#  file.copy("MAPS_Dictionary-Protocol.R", file.path(rproject.path,
#            x[i]))
#  print(i)
#  
#}
#
##Storing the files paths for the latest version
#dictionary.files <- list.files(rproject.path, 
#                               pattern = "MAPS_Dictionary_v2.5.csv",
#                               recursive = TRUE) 
#
##a loop to check that paths are created
#for(i in 1:length(dictionary.files)){
#
#file.path(rproject.path,
#          str_replace(dictionary.files[i],
#          "MAPS_Dictionary_v2.5.csv", "MAPS_Dictionary_v2.6.csv"))
#print(i)
#
#}
#
#
##The loop that actually create the new file in each folder. 
#
#for(i in 1:length(dictionary.files)){
#
#  dictionary.df %>% 
#  write.csv(file.path(rproject.path,
#                str_replace(dictionary.files[i],
#          "MAPS_Dictionary_v2.5.csv", "MAPS_Dictionary_v2.6.csv")),
#   row.names = F)
#
#  print(i)
#}
#
#
##IMPORTANT: This file needs to be updated in Teams!!
##This will only be necessary when a new release is made. 
#
##Saving a copy of the master file for MAPS
#
#dictionary.df  %>% filter(str_detect(ID_3, "\\b")) %>% 
#  select(ID_0:FoodName_1, ID_3, FoodName_3) %>% 
#  rename(
#    food_group_id = "ID_0",
#    food_group_name = "FoodName_0",
#    food_subgroup_id = "ID_1",
#    food_subgroup_name = "FoodName_1",
#    food_genus_id = "ID_3",
#    food_genus_name = "FoodName_3") %>% 
#  write.csv(here::here('output',
#                       'MAPS_Dictionary_master-file_v2.6.csv'), row.names = F)
#