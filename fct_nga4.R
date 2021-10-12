
#loading the packages

library(tidyverse)

## loading data

source("dictionary.R")
source("wafct.R")

fbs <- read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv"))

read.csv(here::here("data", "nga4_foodlist_raw.csv")) %>% 
separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
  mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+")) %>% 
  relocate(nga4_foodid, .before = nga4_fooditem)


nga4.foodlist <- read.csv(here::here("data", "nga4_foodlist_raw.csv")) %>% 
  separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
  mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+"),
         nga4_fooditem = str_remove(nga4_fooditem, "\\d+")) %>% 
  mutate_at("nga4_fooditem", str_squish) %>% 
  relocate(nga4_foodid, .before = nga4_fooditem)




## add the WAFCT id

nga4.foodlist %>% distinct(nga4_foodgroup)

nga4.foodlist %>% filter(nga4_foodgroup == "BEVERAGES") 

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
#Buns, poff and donuts[27] - fried dough NA
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
#animal fat [56] - NA
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
#Sugar - used unfortified but there is one fortified for Nigeria (Vit.A)
#Unground Ogbono [142]
#Ground Ogbono [143]
#Pepper - not powder
#Melon seed (e- for all 2 [145, 146, 147] = Egusi
#Water - only tap 
#Juice canned, sweetened - but it can be averaged of different juices
#Pito - sorghum beer, we could do an average with millet



nga4_wafct_list <- c("01_039", "01_017", "01_037", "01_036", "01_058", "02_036",
                     "02_004", "01_043",
  "04_076", "04_076", "01_006", NA, "01_046", "01_187" , NA, "01_188", NA , 
  "02_001", "02_034", "02_039", NA, "02_005", "02_046", "02_022", "02_009",
  NA, "03_008", "03_027", "03_022", "06_010", "06_010", NA, "06_004", "06_018", 
  "06_001", "11_004", "11_029","11_003" , NA, "11_008", NA, "05_003", "05_016",
  "05_037",
  "05_002", "05_018", "05_034", NA,  "05_017", "05_022", "05_026", "05_010", 
  "04_021",
  "04_066", "04_018", "04_074", "04_017", "04_077", "04_046", "13_006", NA,  NA, 
  NA, NA, "07_070", "08_001", "08_005", NA, "07_002", "07_004", "07_006", 
  "07_046", "07_027", NA, 
  NA, "09_003", NA, "09_053", "07_083", NA, "09_109", NA, "10_029", "10_002", 
  "10_011" , "10_016", "10_028", NA, "12_005", NA, "12_008", "13_002", 
  "13_001", NA, "13_015", NA, NA, "13_014", "06_035", "06_035", "06_035", NA, 
  NA, NA, NA, "12_024", "12_012", NA, "12_001", "12_006", "12_004", NA, NA)

which(is.na(nga4_wafct_list))


#Buns, poff and donuts [27] - fried dough 


#Meat Pie/Sausage Roll [29] need to combine bread/roll (01_045) 
#and meat sausage  (07_063)

meat.pie <- c("01_045", "07_063")

wafct %>% filter(code %in% meat.pie) %>% 
  summarise(across(ENERC2:PHYTCPPD_PHYTCPPI, mean)) %>% 
  cbind(., wafct %>% filter(code %in% meat.pie) %>% 
  summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))

#Gari - yellow [33] - Gari/Eba - fermented cassava, flour with palm oil (yellow) - 
#Need to find oil % (https://scialert.net/fulltext/?doi=pjn.2009.1512.1516)
#gari + red palm oil
#Calculated - fat uptake = 5 for 100g of potato (Bognár, 2002) 
#Table 31: deep fry B
#See markdown for more details.
#02_087 - fufu of yam (VITA_RAE == 499)

gari.yellow <- c("02_039", "11_004")

wafct %>% filter(code %in% gari.yellow) %>%  #change here
  mutate(recipe.conv = c( 1-(5/105), (5/105))) %>% 
  summarise(across(ENERC2:PHYTCPPD_PHYTCPPI, ~sum(.x*recipe.conv, na.rm = F))) %>% 
  cbind(., wafct %>% filter(code %in% fresh.leaves) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))


mutate_at("ENERC2":"PHYTCPPD_PHYTCPPI", ~(. %*% recipe.conv)) %>% head()

#animal fat [56] - 
 

#there are 23 different leaves [78]

wafct %>% filter(str_detect(fooditem, "Leaves|leaves")) %>%
  filter(str_detect(fooditem, "fresh")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% knitr::kable()

fresh.leaves <-  wafct %>% filter(str_detect(fooditem, "Leaves|leaves")) %>%
  filter(str_detect(fooditem, "fresh")) %>%
  filter(str_detect(fooditem, "raw")) %>% pull(code)

wafct %>% filter(code %in% fresh.leaves) %>%  #change here
  summarise(across(ENERC2:PHYTCPPD_PHYTCPPI, mean)) %>% 
  cbind(., wafct %>% filter(code %in% fresh.leaves) %>%  #change here
          summarise(across(c(fooditem, code), ~paste(.x, collapse = ";"))))


wafct %>% filter(code %in% fresh.leaves) %>% 
  ggplot(aes(ZN)) + geom_histogram()

#chicken - do an average of different cuts 
wafct %>% filter(str_detect(fooditem, "Chicken")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% knitr::kable()

#Seafood - do average [105]

wafct %>% 
  filter(str_detect(fooditem, "crab|Crab|lobster|Lobster|Prawn|prawn|shrimp|Shrimp")) %>%
  filter(str_detect(fooditem, "raw")) %>%
  select(code, fooditem) %>% knitr::kable()

#Fresh fish [100], we need to do average or disaggregate.

#Canned fish

wafct %>% filter(str_detect(code, "09_")) %>%
  filter(str_detect(fooditem, "canned")) %>%
  select(code, fooditem) %>% knitr::kable()




## check values for Zn, Fe


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


