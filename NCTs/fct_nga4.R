
################################################################################
#                                                                              #
#                                                                              #
#      Script for matching NLSS food list with food composition data        # 
#                                                                              #
#                                                                              #
#  Data source citation: "Nigeria National Bureau of Statistics. Living        #
#  Standards Survey (NLSS) 2018/19. DOI https://doi.org/10.48529/8gbe-z155     #
#  Ref:NGA_2018_LSS_v01_M                                                      #
#  Dataset downloaded from                                                     #
#  https://microdata.worldbank.org/index.php/catalog/3827 on 12/03/2024        #                                                              #
#                                                                              #
################################################################################


# Loading the packages

#library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

# Loading fct data ----
source("MAPS_fct_load.R")

# source("dictionary.R")
# source("wafct.R")
# source("kenfct.R")
# source("mafood.R")
# source("fct_usda.R")

# Household survey name
hces <-  "lss" 
# Key variables of interest
nut <- c( "WATERg", "ENERCkcal", "VITA_RAEmcg", "FEmg", "ZNmg")

# Loading food data ----
# Loading fbs data
fbs <- read.csv(here::here("data","old-files", "MAPS_FBS_2014-2018_v1.0.csv"))

# Checking the food item list
# read.csv(here::here("data", "old-files",  "nga4_foodlist_raw.csv")) %>% 
# separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
#   mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+")) %>% 
#   relocate(nga4_foodid, .before = nga4_fooditem)

# Loading the food item list
nga4.foodlist <- read.csv(here::here("data", "old-files", "nga4_foodlist_raw.csv")) %>% 
  separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
  mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+"),
         nga4_fooditem = str_remove(nga4_fooditem, "\\d+")) %>% 
  mutate_at("nga4_fooditem", str_squish) %>% 
  relocate(nga4_foodid, .before = nga4_fooditem)

#head(nga4.foodlist)

# Loading previous matches (v.1.0.0)
nga.matches1 <- read.csv(here::here("inter-output", "nct", 
                                   "nga4-fct-id_v1.0.0.csv")) %>% 
                                rename(fdc_id = "content") # renaming fct id variable
#names(nga.matches)

# Assigning fct id matches to dict codes (for MAPS tool) ----
nga.matches1 %>% 
  left_join(., fct_dict %>% select(source_fct, fdc_id, food_desc, 
                                   Edible_factor_in_FCT)) %>% View() 

nga.matches <- nga.matches1 %>% 
  left_join(., fct_dict %>% select(source_fct, fdc_id, food_desc,  ID_3)) 

# Checking missing dict codes
# Note that some matches are wrong due to same fdc_id between diff. fcts 
# subset(nga.matches, is.na(ID_3))
# Checking missing for WA19 (main FCT for NGA)
# subset(nga.matches, is.na(ID_3) & source_fct == "WA19") 
# Checking the missing ids with fct info
# missing <- subset(nga.matches, is.na(ID_3) & source_fct == "WA19") %>% 
#   pull(fdc_id)
# fct_dict %>% filter(fdc_id %in% missing) %>% View()

# Getting only items with dict code and combining the multi matches
nga.matches <- nga.matches %>% select(nga4_foodid , nga4_fooditem, ID_3) %>% 
  filter(!is.na(ID_3)) %>% group_by(nga4_foodid , nga4_fooditem) %>% 
  summarise(across(c(ID_3),
                   ~paste(.x, collapse = ","))) 


# Updating matches to dictionary (v.2.0.0) ----

## Sorghum (10) ----
n <- which(nga.matches$nga4_foodid == "10")
nga.matches[n,]$ID_3 <-  c("114.01,114.02,114.03")
## Millet (11) -----
n <- which(nga.matches$nga4_foodid == "11")
nga.matches[n,]$ID_3 <-  c("118.03,118.04")
## Cassava flour (18) -----
n <- which(nga.matches$nga4_foodid == "18")
nga.matches[n,]$ID_3 <-  c("23170.01.01,23170.01.02, 23170.01.05")
## Maize (Shelled/Off the cob)(22) -----
n <- which(nga.matches$nga4_foodid == "22")
nga.matches[n,]$ID_3 <-  c("112.01")
## Buns/Pofpof/Donuts (27) -----
n <- which(nga.matches$nga4_foodid == "27")
nga.matches[n,]$ID_3 <-  c("F0022.01,F0022.04,F0022.08")
## Meat Pie/Sausage Roll [29]  -----
n <- which(nga.matches$nga4_foodid == "29")
nga.matches[n,]$ID_3 <-  c("F1232.35,F1232.36")
## Palm oil (50) -----
n <- which(nga.matches$nga4_foodid == "50")
nga.matches[n,]$ID_3 <-  c("21691.14.01,21691.14.02,2165.04,2165.01,2165.02")
## Leaves (cocoyam, spinach, etc.) (78) -----
n <- which(nga.matches$nga4_foodid == "78")
nga.matches[n,]$ID_3 <-  c("1215.02,1290.9.17,1219.01.01,1290.9.19,1290.9.06,1290.9.20,1290.9.21,1290.9.22,1290.9.10,1290.9.15,1290.9.18,1239.01.02,1214.04,1290.9.23,1290.9.07,1215.01,1290.9.02,1290.9.24,1290.9.05,1290.9.25,1290.9.26,1290.9.27,1290.9.28")
## Beef (90) -----
n <- which(nga.matches$nga4_foodid == "90")
nga.matches[n,]$ID_3 <-  c("21111.02.01,21111.02.02,21111.02.03")
## Mutton (91) -----
n <- which(nga.matches$nga4_foodid == "91")
nga.matches[n,]$ID_3 <-  c("21115.01,21115.02")
## Pork (92) -----
n <- which(nga.matches$nga4_foodid == "92")
nga.matches[n,]$ID_3 <-  c("21113.02.03,21113.02.04,21113.02.01")
## Goat (93) -----
n <- which(nga.matches$nga4_foodid == "93")
nga.matches[n,]$ID_3 <-  c("21116.02,21116.03")
## Smoked fish [102] ------
n <- which(nga.matches$nga4_foodid == "102")
nga.matches[n,]$ID_3[1] <-  c("1531.02,1505.04,1505.06,1505.05,1518.01")
## Fish - dried [103]  ------
# change to average of including Shrimp crayfish? - See below
n <- which(nga.matches$nga4_foodid == "103")
nga.matches[n,]$ID_3[1] <-  c("1505.07,1507.01,1505.02,1505.05,1505.00.01")
## Baby milk powder [112]  ------
# change to an unfortified
n <- which(nga.matches$nga4_foodid == "112")
nga.matches[n,]$ID_3 <-  c("23991.01.03,23991.01.04")
## Tea [122]  ------
n <- which(nga.matches$nga4_foodid == "122")
nga.matches[n,]$ID_3 <-  c("23914.02")
# Chocolate drink [121] ----
n <- which(nga.matches$nga4_foodid  == "121")
nga.matches[n,]$ID_3 <-  c("F0666.01,F0666.06")

# Checking the missing items.... ----
nga4.foodlist$nga4_foodid <-  as.integer(nga4.foodlist$nga4_foodid)
anti_join(nga4.foodlist, nga.matches)

# Completing values
nga.matches <- left_join(nga4.foodlist, nga.matches)

# Missing items -----

## Maize (Shelled/On the cob) [21]  ------
n <- which(nga.matches$nga4_foodid == "21")
nga.matches[n,]$ID_3 <-  c("1290.01.01")
## Sheabutter [54]  ------
n <- which(nga.matches$nga4_foodid == "54")
nga.matches[n,]$ID_3 <-  c("21691.03.01")
## Local eggs [84]  ------
n <- which(nga.matches$nga4_foodid == "84")
nga.matches[n,]$ID_3 <-  c("231.03")
## Gin [163]  ------
n <- which(nga.matches$nga4_foodid == "163")
nga.matches[n,]$ID_3 <-  c("2413.01")
## Other domestic poultry [82]  ------
n <- which(nga.matches$nga4_foodid == "82")
nga.matches[n,]$ID_3 <-  c("21170.01.02,21124.01")
## Other fruits (specify) [66]  ------
n <- which(nga.matches$nga4_foodid == "66")
nga.matches[n,]$ID_3 <-  c("1359.9.08,1359.02.01,1314.02")
## 	Other grains and flour (specify) [23]  ------
n <- which(nga.matches$nga4_foodid == "23")
nga.matches[n,]$ID_3 <-  c("23710.04,23710.01")
## 	Other meat (excl. poultry) (specify) [96]  ------
n <- which(nga.matches$nga4_foodid == "96")
nga.matches[n,]$ID_3 <-  c("21513.01")
## 	Other milk products (specify) [115]  ------
n <- which(nga.matches$nga4_foodid == "115")
nga.matches[n,]$ID_3 <-  c("141.03")
## 	Other nuts/seeds/pulses [45]  ------
n <- which(nga.matches$nga4_foodid == "45")
nga.matches[n,]$ID_3 <-  c("1449.9.02,1708.01,1491.02.01,1356.01,1356.02")
## 	Other oil and Fat  [53]  ------
n <- which(nga.matches$nga4_foodid == "53")
nga.matches[n,]$ID_3 <-  c("34550.01")
## 	Other vegetables (fresh or canned) [79] ------
n <- which(nga.matches$nga4_foodid == "79")
nga.matches[n,]$ID_3 <-  c("1232.01,1251.01,1212.03")
## 	Other non-alcoholic drinks [155] ------
n <- which(nga.matches$nga4_foodid == "155")
nga.matches[n,]$ID_3 <-  c("24490.05")


# Checking the changes
#subset(nga.matches, is.na(ID_3))

# Adding weights -----

# Mutli-matches 
nga.matches <- nga.matches %>% 
  separate_longer_delim(ID_3, delim = ",") %>% 
  mutate_at("ID_3", str_squish) %>% 
  group_by(nga4_foodid, nga4_fooditem, nga4_foodgroup) %>% 
  mutate(wt = 1/n()) %>% ungroup()

# Specific weights
# Gari - yellow [33] -----
n <- which(nga.matches$nga4_foodid == "33")
nga.matches[n,]$wt <-  c( 1-(5/105), (5/105))

# Saving into csv for sharing
# write.csv(nga.matches, here::here("inter-output",
#                                   "MAPS_dict-food-list-lss_v1.0.2.csv"),
#           row.names = FALSE)
 

# Saving the food consumption and food matches ----
# Checking version
 current_food <- read.csv(here::here("inter-output", 
               sort(list.files(here::here("inter-output"), 
              paste0("MAPS_dict-food-list-", hces)), decreasing = TRUE)[1])) 
 
 
 if(sum(nga.matches != current_food, na.rm = TRUE)>0){
   stop("Difference in food list file, need new version")
   
 } else {
   
   write.csv(nga.matches,
           here::here("inter-output",
                      paste0("MAPS_dict-food-list-", hces, "_v1.0.3.csv")), 
           row.names = FALSE)
   
 }
 

# Search foods ----
food1 <- "be|drin|wat"
food2 <- "mille|sorg"
food3 <- ""
scie <- "edulis"
# wafct 
fct_dict %>% # filter(source_fct %in% c("WA19", "KE18")) %>% 
  # filter(str_detect(fdc_id, "05_")) %>%
  filter(grepl(food1, food_desc, ignore.case = TRUE)) %>% 
  filter(grepl(food2, food_desc, ignore.case = TRUE)) %>% 
 # filter(grepl(food3, food_desc, ignore.case = TRUE)) %>% 
 # filter(grepl(scie, scientific_name, ignore.case = TRUE)) %>% 
#  filter(as.numeric(WATERg)<50) %>% 
#   filter(as.numeric(PROCNTg)<8) %>% 
  # filter(str_detect(fooditem, "flour")) %>% 
#  filter(!is.na(ID_3)) %>% 
  select(source_fct, fdc_id, food_desc, scientific_name, ID_3, WATERg, PROCNTg,
         VITA_RAEmcg, FATg, FEmg, ZNmg, VITB12mcg)  %>%
  View() 

### END - OLD SCRIPTS (v1.0.0.0) #####

## add the WAFCT id

nga4.foodlist %>% distinct(nga4_foodgroup)

nga4.foodlist %>% filter(nga4_foodgroup == "GRAINS AND FLOURS") 

wafct %>% filter(str_detect(code, "12_")) %>%
  select(code, fooditem) %>% knitr::kable()

wafct %>% 
  filter(str_detect(fooditem, "crab|Crab|lobster|Lobster|Prawn|prawn|shrimp|Shrimp")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% knitr::kable()

wafct %>% filter(str_detect(fooditem, "powder")) %>%  
  select(code, fooditem) %>% knitr::kable()

wafct %>% filter(str_detect(fooditem, "grain|flour")) %>% 
  filter(str_detect(fooditem,
                    "sorghum|millet|rice|maize|yam|cassava|wheat")) %>% 
  select(code, fooditem)


#check "native red"
#Cassava flour, non-fermented!
#Wheat flour, white, unfortified - there are fortified for Nigeria
#maize on the cob = maize seed fresh
#maize yellow whole
#bread white, unfortified,  there's one for toasting
#cake plain, unfortified
#Buns, poff and donuts[27] - fried dough --> MWI FCT
#Meat Pie/Sausage Roll [29] need to combine bread/roll (01_045) and meat sausage  (07_063)
#cassava - white
#yam - took combined cultivars from Nigeria
#Gari/Eba - fermented cassava, flour (white) - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4256576/
#Gari/Eba - fermented cassava, flour with palm oil (yellow) - Need to find oil % (https://scialert.net/fulltext/?doi=pjn.2009.1512.1516)
#cocoyam, white
#plantain - choose ivory flesh unripe but we could do an average of multiples
#sweet potato, pale - we could do multiple
#brown bean = cowpea, brown
#groundnuts are = for shelled and unshelled
#coconut - used fresh, immature but not sure 
#cola nut used fresh
#palm oil - red, but we can change to half/half
#margarine for Nigera - It's fortified (vitA, VitD)!! We can change
#animal fat [56] --> KEN FCT 
#banana - we used white flesh, but could do an average 
#orange only, no tangerine
#mango, usesd pale, but maybe an average will be best
#pawpaw == papaya
#used apple with skin (no colour distinction)
#tomato puree (canned) == tomato, paste, concentrated.
#leaves need a bit of thoughts... [78]
#chicken, an average of different cuts, raw [80]
#other domestic poultry [82] - used guinea fowl
#duck [81] == NA
#agricultural egg == "egg"
#local egg == local breed egg
#beef, Lamb/mutton, pork, goat, moderately fat
#Game meat [94] is dried and salted, should double check w/ consumption!
#Fresh fish [100], we could do an average of all the possible or to 
#try to identify the reporting unit and use it accordingly
#could be mullet and sardines for heaps and
#Fresh fish piece catfish for small and medium, and mormyrid? for big?
#Frozen fish [101] could be mackerel  (see photo aid) - FCT is fillet, so double check EP. 
#We finally decided to use an average of mackerel and sardines. 
#For smoked maybe used average of dried and grilled (see photo aid)
#Snail - I used normal snail because of the name and the image of the photo aid.
#Maybe it should be an average
#Seafood [105] - calculate the average
#Canned fish/seafood [106] - we used mackerel but we could do an average
#Baby milk powder, we picked 3months, but we could do an average
#Double-check because it's fortified.
#Milk tinned unsweetened == evaporated milk
#Coffee [120] - powder
#Chocolate drinks [121] - Ovaltine?
#Malt drinks [153] == carbonated drink
#Soft drinks (Coca Cola, Mirinda, etc) [153] == carbonated drink 
#Sugar - used unfortified but there is one fortified for Nigeria (Vit.A)
#Unground Ogbono [142] == 06_008 - Dikanut (Ene-Obong et al.,)
#Ground Ogbono [143] == 06_008 - Dikanut - Not powder 
#Pepper - not powder
#Melon seed (e- for all 2 [145, 146, 147] = Egusi
#Water - only tap (used for bottle and sachet)
#Juice canned, sweetened - but it can be averaged of different juices
#Pito - sorghum beer, we could do an average with millet
#gin [163] - USDA
#check cassava flour fermented?

nga4_wafct_list <- c("01_039", "01_017", "01_037", "01_036", "01_058", "02_036",
                     "02_004", "01_043",
  "04_076", "04_076", "01_006", NA, "01_046", "01_187" , NA, "01_188", NA , 
  "02_001", "02_034", "02_039", NA, "02_005", "02_046", "02_022", "02_009",
  NA, "03_008", "03_027", "03_022", "06_010", "06_010", NA, "06_004", "06_018", 
  "06_001", "11_004", "11_011","11_003" , NA, "11_008", NA, "05_003", "05_016",
  "05_037",
  "05_002", "05_018", "05_034", NA,  "05_017", "05_022", "05_026", "05_010", 
  "04_021",
  "04_066", "04_018", "04_074", "04_017", "04_077", "04_046", "13_006", NA,  NA, 
  NA, NA, "07_070", "08_001", "08_005", NA, "07_002", "07_004", "07_006", 
  "07_046", "07_027", NA, 
  NA, NA , NA, NA, NA, NA, "09_109", NA, "10_029", "10_002", 
  "10_011" , "10_016", "10_028", NA, "12_005", NA, "12_008", "13_002", 
  "13_001", NA, "13_015", "06_008", "06_008", "13_014", "06_035", "06_035", 
  "06_035", NA, "12_019", "12_019", "12_024", "12_024", "12_012", NA, "12_001",
  "12_006", "12_004", NA, NA)

which(is.na(nga4_wafct_list))

wafct_nga4 <- nga4_wafct_list  %>% 
  cbind(., nga4.foodlist) # %>% as.data.frame()
names(wafct_nga4)[1] <- c("fdc_id")

#Buns, poff and donuts [27] - fried dough 
wafct_nga4$nga4_fooditem[wafct_nga4$nga4_foodid=="27"]
wafct_nga4$fdc_id[wafct_nga4$nga4_foodid=="27"] <- "MW01_0014"

# Animal fat [56]
wafct_nga4[56,]
wafct_nga4$fdc_id[56] <-  "9001"
##duck [81] - correction
wafct_nga4[81,]
wafct_nga4$fdc_id[81] <- "7008"
#gin [163]
wafct_nga4[112,]
wafct_nga4$fdc_i[112] <-  "174815"

nga4.foodlist %>% cbind(., nga4_wafct_list %>% as.data.frame() %>% 
                       #   rename(code = ".") %>% 
                          rename(fdc_id = ".") %>% 
#  left_join(., wafct) %>%
  left_join(., fct_cover) %>%
 # select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN)) %>% 
  select(fdc_id, food_desc, WATERg, ENERCkcal, VITA_RAEmcg, FEmg, ZNmg)) %>% 
  filter(is.na(code)) %>% 
  filter(!str_detect(nga4_fooditem, "Other"))
  
# nut <- c( "WATER", "ENERC1", "VITA_RAE", "FE", "ZN")

#Buns, poff and donuts [27] - fried dough 
nga4_adj_fct <-  mwi_clean %>% filter(code =="MW01_0014") %>%
  select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN) %>% 
  mutate(nga4_foodid = "27", 
         comment = NA) %>% mutate_at(nut, as.numeric)


#Meat Pie/Sausage Roll [29] need to combine bread/roll (01_045) 
#and meat sausage  (07_063)

meat.pie <- c("01_045", "07_063")

nga4_adj_fct <- nga4_adj_fct %>% add_row(wafct %>% filter(code %in% meat.pie) %>% 
  summarise(across(nut, mean)) %>% 
  cbind(., wafct %>% filter(code %in% meat.pie) %>% 
  summarise(across(c(fooditem, code), ~paste(.x, collapse = ";")))) %>% 
  mutate(nga4_foodid = "29"))

# Gari/Eba - fermented cassava, flour with palm oil (yellow) - 
#Need to find oil % (https://scialert.net/fulltext/?doi=pjn.2009.1512.1516)
#gari + red palm oil
#Calculated - fat uptake = 5 for 100g of potato (Bognár, 2002) 
# Table 31: deep fry B
# See markdown for more details.
# 02_087 - fufu of yam (VITA_RAE == 499)
gari.yellow <- c("02_039", "11_004")

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% gari.yellow) %>%  #change here
  mutate(recipe.conv = c( 1-(5/105), (5/105))) %>% 
  summarise(across(nut, ~sum(.x*recipe.conv, na.rm = F))) %>% 
  cbind(., wafct %>% filter(code %in% gari.yellow) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";")))) %>% 
  mutate(nga4_foodid = "33"))

# Animal fat [56] 

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(kenfct %>% filter(code== "9001") %>% 
  select(code, fooditem, WATER, nut) %>% 
    mutate(nga4_foodid = "56"))

#there are 23 different leaves [78]

wafct %>% filter(str_detect(fooditem, "Leaves|leaves")) %>%
  filter(str_detect(fooditem, "fresh")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem, nut) %>% knitr::kable()

fresh.leaves <-  wafct %>% filter(str_detect(fooditem, "Leaves|leaves")) %>%
  filter(str_detect(fooditem, "fresh")) %>%
  filter(str_detect(fooditem, "raw")) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% fresh.leaves) %>%  #change here
  summarise(across(nut, mean)) %>% 
  cbind(., wafct %>% filter(code %in% fresh.leaves) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";")))) %>% 
  mutate(nga4_foodid = "78"))


wafct %>% filter(code %in% fresh.leaves) %>% 
  ggplot(aes(ZN)) + geom_histogram()

#chicken [80] - do an average of different cuts, no offal

wafct %>% filter(str_detect(fooditem, "Chicken")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  filter(str_detect(fooditem, "meat")) %>%
  select(code, fooditem) %>% knitr::kable()

chicken.meat <- wafct %>% filter(str_detect(fooditem, "Chicken")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  filter(str_detect(fooditem, "meat")) %>%
  select(code, fooditem) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% chicken.meat) %>%  #change here
  summarise(across(nut, mean)) %>% 
  cbind(., wafct %>% filter(code %in% chicken.meat) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
    mutate(nga4_foodid = "80"))

#duck [81]

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(kenfct %>% filter(code== "7008") %>% 
  select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN) %>% 
    mutate(nga4_foodid = "81"))

#goat [93]

wafct %>% filter(str_detect(fooditem, "Lamb ")) %>% 
  filter(str_detect(fooditem, "raw"))%>% select(code, fooditem, FE, ZN) %>% 
  summarise(across(where(is.numeric), mean))

#14 = 100% 
#12 = meat (86%)
#2 = offals (14%)

#07_046 - Goat meat, moderately fat, ca. 10% fat, raw

#FE = 2.65
(6*.14)+(2.2*.86)

#ZN = 3.24
(2.32*.14)+(3.39*.86)


#Mutton [91]

#Fresh fish [100], we need to do average 

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% knitr::kable()

fresh.fish <- wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>%
  filter(str_detect(fooditem, "raw")) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% fresh.fish) %>%  #change here
            summarise(across(nut, mean)) %>% 
            cbind(., wafct %>% filter(code %in% fresh.fish) %>%  #change here
                    summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
            mutate(nga4_foodid = "100"))  #change here

#Frozen fish [101] -----
# average between mackerel and sardines. 

wafct %>% 
  filter(str_detect(fooditem, "Mackerel|mackerel|Sardine|sardine")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem, WATER, FE, ZN, VITA_RAE) %>% knitr::kable()

frozen.fish <- wafct %>% 
  filter(str_detect(fooditem, "Mackerel|mackerel|Sardine|sardine")) %>%
  filter(str_detect(fooditem, "raw")) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% frozen.fish) %>%  #change here
            summarise(across(nut, mean)) %>% 
            cbind(., wafct %>% filter(code %in% frozen.fish) %>%  #change here
                    summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
            mutate(nga4_foodid = "101"))  #change here



#Smoked fish [102] ------
#we need to do average or disaggregate
#Maybe using grilled instead of smoked?
#filter(str_detect(fooditem, "Sardine|Mullet|Catfish"))
n <- which(nga4.matches$nga4_foodid == "102")
nga4.matches[n,]$ID_3 <-  list(c("1531.02" ,"1505.04" ,"1505.06", 
                                 "1505.05" ,"1518.01"))

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>% 
  filter(str_detect(fooditem, "smoke|grill")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% knitr::kable()

mwi_clean %>% filter(str_detect(fooditem, "Fish")) %>% 
  filter(str_detect(fooditem, "smoke")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>% 
  filter(str_detect(fooditem, "smoke|grill")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 
  
water.conversion <- function(x,y,z){x*(100-z)/(100-y)}

y <- wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>% 
  filter(str_detect(fooditem, "smoke|grill")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(grill_fish_water = mean(WATER)) %>% pull(.)

z <- mwi_clean %>% filter(str_detect(fooditem, "Fish")) %>% 
  filter(str_detect(fooditem, "smoke")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(smoke_fish_water = mean(WATER)) %>% pull(.)

smoked.fish <- wafct %>% filter(str_detect(code, "09_")) %>%
  filter(!str_detect(fooditem, "Crab|Shrimp|clams|Mola|snail")) %>% 
  filter(str_detect(fooditem, "smoke|grill")) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% smoked.fish) %>%  #change here
            summarise(across(nut, mean)) %>%
            mutate(across(nut, ~water.conversion(.x,y,z))) %>% 
            mutate(WATER = z) %>% 
            cbind(., wafct %>% filter(code %in% smoked.fish) %>%  #change here
                    summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
            mutate(nga4_foodid = "102", #change here
                   comment = "water adjusted to average of water content of Malawi FCT smoked fish"))  


# Fish - dried [103]  ------
# change to average of including Shrimp crayfish? - See below
n <- which(nga4.matches$nga4_foodid == "103")
nga4.matches[n,]$ID_3 <-  list(c("1505.07", "1507.01", "1505.02", 
                                 "1505.05", "1505.00.01"))

read.csv(here::here("data", "bogard-2015_fish-composition_incomplete-dataset.csv")) %>% 
  janitor::clean_names() %>% filter(total_vitamin_a_mcg_rae < 500) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

read.csv(here::here("data", "bogard-2015_fish-composition_incomplete-dataset.csv")) %>% 
  janitor::clean_names() %>% filter(total_vitamin_a_mcg_rae < 500) %>% 
  ggplot(aes(total_vitamin_a_mcg_rae)) + geom_histogram()

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "dried")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN)

mwi_clean %>% filter(str_detect(fooditem, "Fish")) %>%
  filter(str_detect(fooditem, "dried")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN)

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "dried")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

################################################################################
# we decided not to use this average of dried fish + crayfish because it was 
# resulting in a very high FE average and it was due to crayfish, which is not
# consumed across the whole country but in the Southern region. This will be good 
# for future analysis. 
#
#
# dried.fish <- wafct %>% filter(str_detect(code, "09_")) %>%
#  filter(str_detect(fooditem, "dried")) %>% pull(code)
#
# nga4_adj_fct <- nga4_adj_fct %>% 
#  add_row(wafct %>% filter(code %in% dried.fish) %>%  #change here
#            summarise(across(nut, mean)) %>% 
#            cbind(., wafct %>% filter(code %in% dried.fish) %>%  #change here
#                    summarise(across(c(fooditem, code),
#                      ~paste(.x, collapse = ";"))))%>% 
#            mutate(nga4_foodid = "103"))  #change here
#
###############################################################################


dried.fish_water <- wafct %>% filter(str_detect(code, "09_")) %>%
   filter(str_detect(fooditem, "dried")) %>% 
  summarise(dried_fish_water = mean(WATER)) %>% pull(.)


fresh.fish_water <- wafct %>% filter(code %in% fresh.fish) %>%
  summarise(fresh_fish_water = mean(WATER)) %>% pull(.)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% fresh.fish) %>%  #change here
            summarise(across(nut, mean)) %>%
            mutate(across(nut, ~water.conversion(.x,fresh.fish_water,dried.fish_water))) %>% 
            mutate(WATER = dried.fish_water) %>% 
            cbind(., wafct %>% filter(code %in% fresh.fish) %>%  #change here
                    summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
            mutate(nga4_foodid = "103", #change here
                   comment = "water adjusted to average of water content of average dried fish"))  



#Snail [104]

wafct %>%
  filter(str_detect(fooditem, "Snail|snail")) %>% 
  filter(str_detect(fooditem, "raw")) %>% 
  select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN)


snail <- wafct %>%
  filter(str_detect(fooditem, "Snail|snail")) %>% 
  filter(str_detect(fooditem, "raw")) %>%  pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% snail) %>%  #change here
            summarise(across(nut, mean)) %>% 
            cbind(., wafct %>% filter(code %in% snail) %>%  #change here
                    summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))%>% 
            mutate(nga4_foodid = "104"))  #change here


#Seafood - do average [105] - not including Clams because its high FE content
#it might bias the average. 

wafct %>% 
  filter(str_detect(fooditem, "crab|Crab|lobster|Lobster|Prawn|prawn|shrimp|Shrimp")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% knitr::kable()

seafood <- wafct %>% 
  filter(str_detect(fooditem, "crab|Crab|lobster|Lobster|Prawn|prawn|shrimp|Shrimp")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% seafood) %>%  #change here
  summarise(across(nut, mean)) %>% 
  cbind(., wafct %>% filter(code %in% seafood) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";")))) %>% 
    mutate(nga4_foodid = "105"))  #change here

##Canned fish/seafood [106] - we used mackerel but we could do an average
#particularly because mackerel is very high in VITA_RAE

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "canned")) %>%
  select(code, fooditem) %>% knitr::kable()

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "canned")) %>%
  select(code, fooditem, WATER, VITA_RAE, FE, ZN)

canned.fish <- wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "canned")) %>%
  pull(code)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(wafct %>% filter(code %in% canned.fish) %>%  #change here
  summarise(across(nut, mean)) %>% 
  cbind(., wafct %>% filter(code %in% canned.fish) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";")))) %>% 
    mutate(nga4_foodid = "106"))  #change here

# Chocolate drink [121] ----
n <- which(nga4.matches$nga4_foodid == "121")
nga4.matches[n,]$ID_3 <-  list(c("23170.01.04", "2165.04"))
c("F0666.02" ,"22290.06", "22290.07", "F0666.03" ,"22290.08",
  "22290.09", "22290.10"


# ken - choco 12004 ---> this need conversion to 
# ovaltine beverage water content (12_015)

# kenfct

fct_dict %>% filter(fdc_id =="12004") %>%
  select(fdc_id, food_desc, nut) %>% 
  mutate(across(c(ENERC1, VITA_RAE, FE, ZN), ~water.conversion(.x,WATER, ovaltine_water))) %>% 
  mutate(WATER = ovaltine_water) 

wafct %>% filter(str_detect(code, "12_")) %>%
  filter(str_detect(fooditem, "Oval")) %>% 
  select(code, fooditem, WATER, VITA_RAE, FE, ZN)

ovaltine_water <- wafct %>% filter(str_detect(code, "12_")) %>%
  filter(str_detect(fooditem, "Oval")) %>% 
  select(code, fooditem, WATER, VITA_RAE, FE, ZN) %>% 
  summarise(mean(WATER)) %>% pull(.)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(kenfct %>% filter(code =="12004") %>%
  select(code, fooditem, nut) %>% 
  mutate(across(c(ENERC1, VITA_RAE, FE, ZN), ~water.conversion(.x,WATER, ovaltine_water))) %>% 
  mutate(WATER = ovaltine_water) %>% 
  mutate(nga4_foodid = "121", 
         comment = "water adjusted to average of water content of average Ovaltine beverage with milk"))  #change here

#gin [163]

usdafct %>% filter(code == "174815")%>% 
  select(code, fooditem, WATER, VITA_RAE, FE, ZN)

nga4_adj_fct <- nga4_adj_fct %>% 
  add_row(usdafct %>% filter(code == "174815") %>% 
           select(code, fooditem,nut) %>% 
            mutate(nga4_foodid = "163"))


#Combining all food items together

nga4.foodlist %>% cbind(., nga4_wafct_list %>% as.data.frame() %>%  rename(code = ".") %>% 
                          left_join(., wafct) %>%
                          select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN)) %>% 
  filter(is.na(code)) %>% 
  filter(!str_detect(nga4_fooditem, "Other")) %>% select(starts_with("nga4_")) %>% 
  left_join(., nga4_adj_fct)

fct_nga4 <-  nga4.foodlist %>% cbind(., nga4_wafct_list %>% as.data.frame() %>%  rename(code = ".") %>% 
                          left_join(., wafct) %>%
                          select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN)) %>% 
  filter(!is.na(code)) %>% 
  mutate(comment = NA) %>% 
  rbind(nga4.foodlist %>% cbind(., nga4_wafct_list %>% as.data.frame() %>%  rename(code = ".") %>% 
                                  left_join(., wafct) %>%
                                  select(code, fooditem, WATER, ENERC1, VITA_RAE, FE, ZN)) %>% 
          filter(is.na(code)) %>% 
          filter(!str_detect(nga4_fooditem, "Other")) %>% select(starts_with("nga4_")) %>% 
          left_join(., nga4_adj_fct))


## check values for Zn, Fe

fct_nga4 %>% ggplot(aes(nga4_foodgroup, FE)) + geom_boxplot() +
  coord_flip()

fct_nga4 %>% filter(nga4_foodgroup == "VEGETABLES",  FE > 10)
fct_nga4 %>% filter(nga4_foodgroup == "MILK AND MILK PRODUCTS",  FE > 5)
fct_nga4 %>% filter(nga4_foodgroup == "MEAT",  FE > 5)
fct_nga4 %>% filter(nga4_foodgroup == "GRAINS AND FLOURS",  FE > 5)

fct_nga4 %>% ggplot(aes(nga4_foodgroup, ZN)) + geom_boxplot() +
  coord_flip()

fct_nga4 %>% filter(nga4_foodgroup == "VEGETABLES",  ZN > 4)
fct_nga4 %>% filter(nga4_foodgroup == "STARCHY ROOTS, TUBERS & PLANTAIN",  ZN > 2)
fct_nga4 %>% filter(nga4_foodgroup == "MEAT",  ZN > 4)

fct_nga4 %>% filter(VITA_RAE <900) %>% 
  ggplot(aes(nga4_foodgroup, VITA_RAE)) + geom_boxplot() +
  coord_flip()

fct_nga4 %>% filter(nga4_foodgroup == "OIL AND FATS",  VITA_RAE > 750)
fct_nga4 %>% filter(nga4_foodgroup == "VEGETABLES",   VITA_RAE > 800)
fct_nga4 %>% filter(nga4_foodgroup == "STARCHY ROOTS, TUBERS & PLANTAIN", VITA_RAE > 200)
fct_nga4 %>% filter(nga4_foodgroup == "OTHER MISCELLANEOUS FOODS", VITA_RAE > 50)


#Saving fct_nga4 into csv

fct_nga4 %>% write.csv(here::here("output", "fct_nga_v1.0.csv"), row.names = F)

## Food matching for MAPS tool - GENuS code


dictionary.df %>% filter(ID_3 %in% c("23120.03.01", "23120.03.02","112.01","1290.01.01",
  "118.02","114.01","118.03","23110.01","23110.02")) %>% select(ID_3, FoodName_3)

## add the food genus code to each item

nga4.foodlist %>% distinct(nga4_foodgroup)

nga4.foodlist %>% filter(nga4_foodgroup == "MEAT") 

dictionary.df %>% filter(str_detect(FoodName_3, "grain|flour")) %>% 
  filter(str_detect(FoodName_3,
                    "sorghum|millet|rice|maize|yam|cassava|wheat")) %>% 
  select(ID_3, FoodName_3)

dictionary.df %>% filter(str_detect(FoodName_3, "yam")) %>% 
  select(ID_3, FoodName_3)

#When no info was provided, the most refined option was taken
#maize off the cob = white, dried
#maize on the cob = green maize, both shelled and unshelled are the same
#just differed in EP


c("114.01", "118.01", "23161.02.01", "23161.01.01", 
  "23120.03.02", NA, "23170.01", "23110.02", "1290.01.01", "1290.01.01" , 
  "112.01", NA  )


