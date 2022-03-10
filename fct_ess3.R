

################################################################################
#  
#
#      Script for matching ESS3 food list with food composition data
#
#
# Data source citation: "Central Statistical Agency of Ethiopia. Ethiopia 
#  Socioeconomic Survey,Wave 3 (ESS3) 2015-2016. Public Use Dataset. 
#  Ref: ETH_2015_ESS_v02_M. 
#  Downloaded from https://microdata.worldbank.org/index.php/catalog/2783
#  on 21/12/2-21"
#
################################################################################



#0) Loading r packages

library(tidyverse)


#1) Loading dataset

read.csv(here::here("data", "sect5a_hh_w3.csv"))

#Getting the list of foods in the questionnaire
#hh_s5aq01 (consumed yes (1) or no (2))

foodlist <-read.csv(here::here("data", "sect5a_hh_w3.csv")) %>% 
  select(item_cd, hh_s5aq0a, hh_s5aq01) %>% filter(hh_s5aq01 == "1") %>% 
  count(item_cd, hh_s5aq0a) %>%  #duplicate 
  rename(foodid = "item_cd",
         fooditem= "hh_s5aq0a") %>% 
  arrange(desc(n)) %>% 
  arrange(foodid)

foodlist <- read.csv(here::here("data", "sect5a_hh_w3.csv")) %>% 
  select(item_cd, hh_s5aq0a, hh_s5aq01) %>% distinct() %>% slice(1:55) %>%  #duplicate 
  rename(foodid = "item_cd",                                   #9, 111, 143
         fooditem= "hh_s5aq0a")

#Identifying the "other" foods and arranged by number of HH 
#reporting consuming them. 

read.csv(here::here("data", "sect5a_hh_w3.csv")) %>% 
  select(hh_s5aq0a_others) %>% count(hh_s5aq0a_others) %>% 
  arrange(desc(n))

#2) Food Genus matching. 

#Reading from the Dictionary

source("dictionary.R")

dictionary <- dictionary.df %>%
  rename(fooditem = FoodName_3) 

Encoding(dictionary$fooditem)<-"latin1" 

dict_testsample = dictionary[,c(7,9)]   

fuzzy_output <- stringdist_join(foodlist, dict_testsample, #This selects the two lists to check matches against
                               by = "fooditem", #This allows you to select the field by which the search will be done
                               mode = "left",
                               method = "jw", #the fuzzy search method - more info here, need to do some research
                               ignore_case=TRUE,
                               max_dist = 10, #The maximum distance between the two strings - I believe this varies dependent on the method
                               distance_col = "dist") %>% #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  group_by(fooditem.x) %>% #output formatting - this makes it so that the output is organised by the fooditem.x, (x being the first list item at the start of the tool, Kenfct)
  slice_min(order_by = dist, n = 5) #This means only the closest 5 matches are listed per food item on the FCT

#Checking the accuracy of the matches by distance

fuzzy_output %>%
  arrange(dist) %>%  knitr::kable()

ess_food <- tribble(
  ~ref_foodid, ~ref_fooditem,   ~ID_3, ~confidence,
   
  
  "1", "Teff",  "1199.9.01", "h", 
  "2", "Wheat", "111.01", "l", 
  "2", "Wheat", "111.02", "l", 
  "3", "Barley", "115.01", "l", 
  "4", "Maize",  "112.01", "l", 
  "4", "Maize",  "112.02", "l", 
  "5", "Sorghum", "114.01", "m",
  "6", "Millet", "118.01", "m", 
  "7", "Horsebeans", "1702.01", "m", 
  "8", "Field Pea", "1705.01", "m", 
  "9", "Chick Pea", "1703.01", "h", 
  "10", "Lentils", "1704.01", "m", 
  "11", "Haricot Beans" ,"1701.02", "m", 
  "12", "Niger Seed", "1449.9.01", "h", 
  "13", "Linseed", "1441.01", "h", 
  "14", "Onion",  "1253.02.01", "h", 
  "15", "Banana", "1312.01", "h",
  "16", "Potato", "1510.01", "h", 
  "17", "Kocho", "23170.02.01", "h", 
  "19", "Milk",  "2211.01", "m", 
  "20", "Cheese", "22251.01.01", "m", 
  "21", "Eggs", "231.01", "h",
  "22",  "Sugar", "23520.01", "m", 
  "23", "Salt",        "1699.02", "h", 
#  "24",  "Coffee",       "23912.02.02", "l", 
   "24",  "Coffee",       "23912.02.01", "l", 
  "25", "Chat / Kat", NA, NA, 
  "26", "Bula", "23170.02.02", "h", 
  "60", "Other cereal (SPECIFY)", NA, NA,   
  "110", "Ground nuts", "142.01", "m",
  "111", "Other pulse or nut (SPEC", NA, NA, 
  "131",  "Other seed (SPECIFY)", NA, NA, 
  "141", "Green chili pepper (kari", "1652.01", "m",
  "142", "Red pepper (berbere)", "1652.02", "m", 
  "143", "Greens (kale, cabbage, e", "1212.01", "l", 
  "143", "Greens (kale, cabbage, e", "1215.01", "l", 
  "144",  "Tomato",       "1234.01", "m",                    
  "145", "Other vegetable (SPECIFY", NA, NA, 
  "151",  "Orange",       "1323.01", "m", 
  "152", "Other fruit (SPECIFY)", NA, NA,
  "170",  "Sweet potato",  "1530.01", "m", #white-flesh
  "171", "Boye/Yam", "1540.01", "m", 
  "172",  "Cassava",       "01520.01.01", "m", 
  "173", "Godere", "1550.01", "m", 
  "174", "Other tuber or stem (SPE", NA, NA, 
  "180", "Goat & mutton meat", "21116.01", "m", 
  "180", "Goat & mutton meat", "21115.01", "m", 
  "181", "Beef",      "21111.02.01", "l", 
  "182",  "Poultry", "21121.01", "l", 
  "183", "Fish", "1501.05", "l", 
  "195", "PuUrchased Injera", "23140.08.01", "h",
  "196", "Purchased Bread or Biscu", "F0020.01", "l", 
  "196", "Purchased Bread or Biscu", "F0022.03", "l", 
  "197",  "Pasta/Maccaroni", "23710.01", "h", 
  "198",  "Other prepared food and", NA, NA, 
  "201",  "Butter/ghee",  "22241.01.01", "m", 
  "201",  "Butter/ghee",  "22241.02.01", "m", 
  "202",   "Oils (processed)", "2165.01", "l",
 # "203",  "Tea",         "23914.01", "l", 
  "203",  "Tea",         "23914.02", "l", 
  "204",   "Soft drinks/Soda" ,"24490.02", "l", 
  "205",  "Beer",        "24310.01.01", "m",
  "206", "Tella", "24310.02.01", "l") %>% 
  left_join(., dictionary.df %>% distinct()) 

# ess_food %>% 
#   write.csv(., here::here("inter-output", "ess3_food-list_matching.csv"),
#             row.names = FALSE)
 
  
  
  fuzzy_output %>% filter(dist > 0.285) %>%
    arrange(dist) 
  
  dictionary %>% filter(str_detect(fooditem, "pea"))
  dictionary %>% filter(str_detect(FoodName_2, "seed"))
  dictionary %>% filter(ID_2 == "23140.08") 
  dictionary %>% filter(ID_1 == "2520")  %>% distinct(FoodName_2)
  dictionary %>% filter(ID_0 == "RT") %>% distinct(FoodName_1)

#New dataset from 2018-2019.
  
ess4_food_list <- read.csv(here::here("inter-output", "eth_fct_match_v1.csv")) %>% 
  select(1, 2) %>% mutate(
    ref_fooditem = str_replace(item_code, "[:digit:]{2,3}\\.", "")) %>% 
  mutate_at("ref_fooditem", str_squish) %>% select(-item_code)


ess4_food <- ess4_food_list  %>%  left_join(., ess_food) %>% 
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
  left_join(., ess_food %>% select(-ref_fooditem)) %>% 
  bind_rows(., ess4_food %>% filter(!is.na(ID_3))) 


ess4_food_list %>% left_join(., ess4_food) %>% filter(is.na(ID_3)) %>% 
  filter( !str_detect(ref_fooditem, "Other"))

#Assigning 

#3) Food matching

#KENFCT, 2018
source("kenfct.R")

no_match <- ess_food %>% filter(!is.na(ID_3)) %>% 
  left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
  filter(is.na(energy_in_kcal)) %>% select(1:3)
  
  
#WAFCT, 2019

source("wafct.R")


