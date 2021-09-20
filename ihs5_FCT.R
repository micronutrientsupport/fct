

library(tidyverse)


AHHA <- read.csv(here::here("data", "AHHA-FCT.csv"))

MAFOODS <- read.csv(here::here("output", "MAPS_MAFOODS_v1.4.csv"))

WAFCT <- source("wafct.R")


c("food_genus_description"    , "food_group"               ,  "food_subgroup" ,
  "food_genus_confidence"    ,  "fct_name"  , "energy_in_kj", 
  "nitrogen_in_g"       ,       "totalprotein_in_g"      ,    "totalfats_in_g" ,
  "saturatedfa_in_g"           "monounsaturatedfa_in_g"     "polyunsaturatedfa_in_g" ,
  "cholesterol_in_mg"          "carbohydrates_in_g"         "fibre_in_g"  ,
  "ash_in_g" , )

#popcorn - AHHA - 1009 


ihs5 <- read.csv("ihs5-fct_v1.1.csv")
#mucuna - WAFCT - 03_059 and Joy et al., 
#unprocess -

 ihs5 %>% 
  relocate(food_genus_id, .after = "ihs5_fooditem") %>% 
  mutate(ref = ifelse(is.na(ref.x), ref.x.x, 
                      ifelse(is.na(ref), ref.x, ref))) %>% 
  select(-c("FoodName_3":"FoodName_2")) %>% 
  select(-ends_with("_code")) %>% 
   select(-ends_with("_fct")) %>% 
   select(-ends_with(".x")) %>% 
  filter(!food_genus_id %in% c("1530.06", "1321.02", "1322.02", "1324.02" )) %>% 
  mutate(food_genus_id = case_when(
    food_genus_id == "24230.03.03|24230.03.02" ~ "24230.03.02",
    food_genus_id == "1290.01.01|1290.01.02" ~ "1290.01.03",
    TRUE ~ food_genus_id)) %>% 
       count(ref_fctcode) %>% arrange(desc(n))
 
#We need to remove some items that are not well represented,
#those are food items with a genus code that did not corresponed to
#a food compo entry the list is a follow:
# "1530.06" ==  to orange, sweet potato
# "1321.02" == pomelo/grapefruit
# "1322.02" == limes,
# "1324.02" == mandarines
# 
# to be checked! 406 - other cultivated leafy
# 
# other issues is wrong coding at consumption level (i.e., applying differente
 #genus_id to same como item. that was the case for 24230.03.02 - maize beer), 
 #same happened w/ green maize
 
 #BEST TO CHECK ALL ITEMS AND MATCHES ONE BY ONE!!!

 # Infant formula is in the file name: food-match-mwi_v02.csv
 #

# #
# #
 
 
 ihs5 %>% 
   relocate(food_genus_id, .after = "ihs5_fooditem") %>% 
   mutate(ref = ifelse(is.na(ref.x), ref.x.x, 
                       ifelse(is.na(ref), ref.x, ref))) %>% 
   select(-c("FoodName_3":"FoodName_2")) %>% 
   select(-ends_with("_code")) %>% 
   select(-ends_with("_fct")) %>% 
   select(-ends_with(".x")) %>% 
   filter(!food_genus_id %in% c("1530.06", "1321.02", "1322.02", "1324.02" )) %>% 
   mutate(food_genus_id = case_when(
     food_genus_id == "24230.03.03|24230.03.02" ~ "24230.03.02",
     food_genus_id == "1290.01.01|1290.01.02" ~ "1290.01.03",
     TRUE ~ food_genus_id)) %>% 
   filter(ih5_itemid != )
   write.csv(.,here::here("ihs5-fct_v1.2.csv"))
 
#food_genus_id    n
#1       1321.02 1249
#2       1322.02 1249
#3       1324.02 1249
#4       1530.06   53
#
#5   21113.02.01  880
#6   23991.01.02   16
#7   39120.04.01  807
   
   
   ## loading data
   
   ihs5 <- read.csv("ihs5-fct_v1.2.csv") %>% select(-X)
   
   source("dictionary.R")
   
#---ihs5-quality-check
   
#1) Checking that ihs5 and genus matches are correct. 

#Creating the list of ihs5 food items and its genus code
  
   
#Fixing samosa vs banana cake id typo issue (see MAPS_Dictionary-Protocol update v2.5)
ihs5$food_genus_id[ihs5$ihs5_foodid == "836" & ihs5$food_genus_id == "F0022.06"] <- "F0022.07"
 
#checking that all the ihs5 food items matches make sense  
ihs5_genus <- ihs5 %>% select(starts_with("ihs5"), food_genus_id) %>% 
 left_join(., dictionary.df %>% select(ID_3, FoodName_3), by = c("food_genus_id" = "ID_3")) %>% 
 arrange(ihs5_foodid)
   
   
#Checked one by one all the matches and noted down the combination of codes that
#did not make sense for further checking

#836 - F0022.06 is no longer printing because solved above
#I left it for record keeping

ihs5_genus %>% 
 filter(ihs5_foodid %in% c("816", "832" , "836", "106", "510") &
          food_genus_id %in% c("F0666.01", "1530.07", "F0022.06",
                                   "23161.01.01", "21119.01.01"))


#removing matches that doesn't make sense in the ihs5-genus match    
ihs5_genus <- ihs5_genus %>%
     filter(!(ihs5_foodid %in% c("816", "832" , "836", "106", "510") &
                food_genus_id %in% c("F0666.01", "1530.07", "F0022.06",
                                     "23161.01.01", "21119.01.01")))

#Saving a copy of the matches so Gareth can check it (and other)
#write.csv(ihs5_genus, 
#          here::here("output", "ihs5-genus_standard-food-list_v2.0.csv"), row.names = F)

#TO-DO:
#Fix fct_ihs5 genus id codes (i.e., 816 - 23670.01.01, 1530.07)
#Remove redundant items (mice, rice imported, etc. ) - this is not very dramatic step, but it would help
#to keep the fct clean.
#Dissagregate aggregated items


#removing matches that doesn't make sense   

   
      genus.double <- ihs5 %>% count(food_genus_id) %>% arrange(desc(n)) %>% 
     filter(n>1) %>% pull(food_genus_id)
   
   ihs5 %>% filter(food_genus_id %in% genus.double) %>% arrange(food_genus_id)
   
   
   genus.double <- ihs5 %>% select(-starts_with("ihs5")) %>% distinct() %>% 
     count(food_genus_id) %>% arrange(desc(n)) %>% 
     filter(n>1) %>% pull(food_genus_id)
   
   
   ihs5 %>% select(-starts_with("ihs5")) %>% distinct() %>%
     filter(food_genus_id %in% genus.double) %>% arrange(food_genus_id)
 
 