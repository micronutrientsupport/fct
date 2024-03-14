
# Loading libraries
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

#Searching terms and correspondences with categories - use CPC
#http://datalab.review.fao.org/datalab/caliper/web/concepts-search

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

####-----New Dictionary codes --------#####

dictionary.df <- read.csv(here::here("metadata", "MAPS_Dictionary_v2.6.csv")) %>% 
  select(-starts_with("X"))

dictionary.df$scientific_name <- NA

colnames(dictionary.df)

dictionary.df %>% filter(str_detect(ID_3, "01520"))

#data corrections for dictionary.df, dict.comp and TZ_data . Seems like there were some spelling mistakes that lead to broken matches.
#dictionary.df <- dictionary.df %>% drop_na(ID_0) # not needed, but maybe a good idea - quite a few all NA rows?
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.01"] <- "1520.01.01"
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.02"] <- "1520.01.02"
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.03"] <- "1520.01.03"

#It was a typo that duplicated two contiguous food items,
#here we are solving it

dictionary.df$ID_3[dictionary.df$FoodName_3 == "cake, banana"] <- "F0022.07"


#---tracking-issue-2

#checking ids duplication at group level 2 (ID_2):
dictionary.df %>% distinct(FoodName_2, ID_2) %>%
  count(ID_2) %>% arrange(desc(n))

#checking what items with duplicated ids
dictionary.df %>% filter(ID_2 %in% c("1510", "1530"))

#checking that the new proposed ids
dictionary.df %>% filter(ID_2 %in% c("15100", "15300"))

#changing fish ids to new ones:

dictionary.df <-  dictionary.df %>% mutate(ID_2 = case_when(
  FoodName_2 == "freshwater fish, liver oil" ~ "15100",
  FoodName_2 == "pelagic fish, frozen, fillet" ~ "15300", 
  TRUE ~ ID_2
))


#New codes coming from FBS and FBSH

#FBS
#2558 == 1442.01 (mustard seed, dried, raw) New
# 2659 == 24110.01 (alcohol, 80perc) New 
#FBSH
#2562 == 1491.02.01 (palm kernel, raw) New

dictionary.df <- dictionary.df %>% 
  # add_row(
  # ID_0 = "OT",
  # FoodName_0 = "Other foods", 
  # ID_1 = 2558,
  # FoodName_1 = "rape and mustardseed and products", 
  # ID_2 = "1442", 
  # FoodName_2 =  "mustard seed",
  # ID_3 = "1442.01",
  # FE2_3 = "A015S#F28.A07KG$F28.A07HS",
  # FoodName_3 = "mustard seed, dried, raw") %>% 
 # add_row(
 #   ID_0 = "OT",
 #   FoodName_0 = "Other foods", 
 #   ID_1 = 2659,
 #   FoodName_1 = "alcohol, non-food and products", 
 #   ID_2 = "24110", 
 #   FoodName_2 =  "undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher",
 #   ID_3 = "24110.01",
 #   FE2_3 = NA,
 #   FoodName_3 = "alcohol, 80perc") %>% 
 # add_row(
 #   ID_0 = "OT",
 #   FoodName_0 = "Other foods", 
 #   ID_1 = 2562,
 #   FoodName_1 = "palm kernels and products", 
 #   ID_2 = "1491.02", 
 #   FoodName_2 = "palm kernels [only calories]",
 #   ID_3 = "1491.02.01",
 #   FE2_3 = "A0DAJ#F28.A07HS",
 #   FoodName_3 = " palm kernel, raw") %>% 
  add_row(
    ID_0 = "FV",
    FoodName_0 = "Fruits and Vegetables", 
    ID_1 = 2617,
    FoodName_1 = "apples and products", 
    ID_2 = "21435.01", 
    FoodName_2 = "apple juice",
    ID_3 = "21435.01.01",
    FE2_3 = "A039M#F10.A077J", # We used qualitative info w/ added sugar to specify that it's sweetened 
    FoodName_3 = " apple juice, sweetened")  #we did not used the #sweetened agent bc we did not know 
#the sweetened used. 


#making a distinction between two rice that were named the same
#we will use local rice whenever possible 
#default rice white == "23161.02.01"

dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.01.01"] <- "rice grain, imported, white, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.02.01"] <- "rice grain, local, white, dried, raw"

#Found a issue with some ID_0 classification
#Need to change them to oil and seeds.

#checking what items with duplicated ids
dictionary.df %>% filter(ID_1 %in% c("2561", "2571", "2572", "2579"))

dictionary.df$ID_0[dictionary.df$ID_1 %in% c("2561", "2571",
                                             "2572","2579")] <- "OT"

dictionary.df$FoodName_0[dictionary.df$ID_1 %in% c("2561", "2571",
                                                   "2572","2579")] <- "Other foods"


#We are adding canned beans, we need to decide where to put them
#Since they are not dried and they are preserved, acc. to FAO
#logic it should be under other vegetables.
#additionally we are adding them to be matched to tinned vegetables, in 
#ihs5....

#food-group fixing 

#sugar
dictionary.df$FoodName_0[dictionary.df$ID_3 == "23511.02.01"] <- "Other foods"
dictionary.df$ID_0[dictionary.df$ID_3 == "23511.02.01"] <- "OT"

#juice
dictionary.df$FoodName_2[dictionary.df$ID_2 == "21435.01"] 
dictionary.df$FoodName_1[dictionary.df$ID_2 == "21435.01"] 
dictionary.df$FoodName_0[dictionary.df$ID_2 == "21435.01"] 

#Adding new entries from Ethiopia HCES - ess3

dictionary.df <- dictionary.df %>%
  add_row(
    ID_0 = "CE",
    FoodName_0 = "Cereals", 
    ID_1 = 2513,
    FoodName_1 = "barley and products", 
    ID_2 = "115", 
    FoodName_2 =  "barley",
    ID_3 = "115.01",
    FE2_3 = "",
    FoodName_3 = "barley grain, dried, unrefined, raw") %>% 
  add_row(
    ID_0 = "PB",
    FoodName_0 = "Pulses and Beans", 
    ID_1 = 2549,
    FoodName_1 = "pulses, other and products", 
    ID_2 = "1703", 
    FoodName_2 =  "chick peas, dry",
    ID_3 = "1703.01",
    FE2_3 = "",
    FoodName_3 = "chick peas, dried, raw") %>% 
  add_row(
    ID_0 = "OT",
    FoodName_0 = "Other foods", 
    ID_1 = 2570,
    FoodName_1 = "oilcrops, other and products", 
    ID_2 = "1449.9", 
    FoodName_2 =  "other oil seeds, n.e.c.",
    ID_3 = "1449.9.01",
    FE2_3 = "",
    FoodName_3 = "niger seeds, dried, raw") %>% 
  add_row(
    ID_0 = "RT",
    FoodName_0 = "Roots and Tubers", 
    ID_1 = 2534,
    FoodName_1 = "roots, other and products", 
    ID_2 = "23170.02", 
    FoodName_2 =  "flour of roots and tubers nes",
    ID_3 = "23170.02.01",
    FE2_3 = "",
    FoodName_3 = "bread, ensete pulp, fermented, raw", 
    Description1 = "kocho: bread-like fermented food made from chopped and grated ensete pulp", 
    Desc1.ref = "https://en.wikipedia.org/wiki/Kocho_(food)") %>% 
  add_row(
    ID_0 = "RT",
    FoodName_0 = "Roots and Tubers", 
    ID_1 = 2534,
    FoodName_1 = "roots, other and products", 
    ID_2 = "23170.02", 
    FoodName_2 =  "flour of roots and tubers nes",
    ID_3 = "23170.02.02",
    FE2_3 = "",
    FoodName_3 = "ensete, flour, raw", 
    Description1 = "bula") # %>% 
 # add_row(
 #   ID_0 = "CE",
 #   FoodName_0 = "Cereals", 
 #   ID_1 = 2520,
 #   FoodName_1 = "cereals, other and products", 
 #   ID_2 = "23140.08", 
 #   FoodName_2 =  "cereal preparations",
 #   ID_3 = "23140.08.01",
 #   FE2_3 = "",
 #   FoodName_3 = "injera, teff grain, ready-to-eat")

#Adding new entries from Ethiopia HCES - ESS4

dictionary.df <- dictionary.df %>%
  add_row(
    ID_0 = "PB",
    FoodName_0 = "Pulses and Beans", 
    ID_1 = 2549,
    FoodName_1 = "pulses, other and products", 
    ID_2 = "1709.01", 
    FoodName_2 =  "vetches",
    ID_3 = "1709.01.01",
    FE2_3 = "",
    FoodName_3 = "vetch, dried, raw") %>% 
  add_row(
    ID_0 = "OT",
    FoodName_0 = "Other foods", 
    ID_1 = 2645,
    FoodName_1 = "spices, other and products", 
    ID_2 = "1699", 
    FoodName_2 =  "other stimulant, spice and aromatic crops, n.e.c.",
    ID_3 = "1699.07",
    FE2_3 = "",
    FoodName_3 = "fenugreek, dried, raw")  %>%
  add_row(
    ID_0 = "PB",
    FoodName_0 = "Pulses and Beans", 
    ID_1 = 2549,
    FoodName_1 = "pulses, other and products", 
    ID_2 = "1709.9", 
    FoodName_2 =  "other pulses n.e.c.",
    ID_3 = "1709.9.01",
    FE2_3 = "",
    FoodName_3 = "mung bean, dried, raw") %>%
  add_row(
    ID_0 = "PB",
    FoodName_0 = "Pulses and Beans", 
    ID_1 = 2549,
    FoodName_1 = "pulses, other and products", 
    ID_2 = "23170.03", 
    FoodName_2 =  "flour of pulses",
    ID_3 = "23170.03.01",
    FE2_3 = "",
    FoodName_3 = "chick peas flour, dried, raw") %>%
  add_row(
    ID_0 = "FV",
    FoodName_0 = "Fruits and Vegetables", 
    ID_1 = 2605,
    FoodName_1 = "vegetables, other and products", 
    ID_2 = "1252", 
    FoodName_2 =  "green garlic", #http://datalab.review.fao.org/datalab/caliper/web/concept-page/0406-garlic
    ID_3 = "1252.01",
    FE2_3 = "",
    FoodName_3 = "garlic, fresh, raw")  %>% 
 # add_row(
 #   ID_0 = "OT",
 #   FoodName_0 = "Other foods", 
 #   ID_1 = 2645,
 #   FoodName_1 = "spices, other and products", 
 #   ID_2 = "1699", 
 #   FoodName_2 =  "other stimulant, spice and aromatic crops, n.e.c.",
 #   ID_3 = "1699.08",
 #   FE2_3 = "",
 #   FoodName_3 = "moringa, leaves, raw") %>%
  add_row(
    ID_0 = "OT",
    FoodName_0 = "Other foods", 
    ID_1 = 2645,
    FoodName_1 = "spices, other and products", 
    ID_2 = "1699", 
    FoodName_2 =  "other stimulant, spice and aromatic crops, n.e.c.",
    ID_3 = "1699.09",
    FE2_3 = "",
    FoodName_3 = "hops, dried, raw") 

### Cereals (CE) ----

#Fixing ID_1 codes 
dictionary.df$ID_1[dictionary.df$ID_1 == "2806"] <- "2805"
#Fixing ID_2 
dictionary.df$ID_2[dictionary.df$ID_2 == "23121.01"] <- "23120.01"
#Improving FoodName_3 description
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23140.05.01"] <- "barley grain, pearl, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "114.01"] <- "sorghum grain, average, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "114.01"] <- "sorghum grain, average, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1193.04"] <- "fonio grain, white, refined, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "118.03"] <- "pearl millet grain, dried, unrefined, raw"

#Adding other names
dictionary.df$Description2[dictionary.df$ID_3 == "23161.01.01"] <- "rice grain, white, polished, dried, raw"
dictionary.df$Description2[dictionary.df$ID_3 == "23161.02.01"] <- "rice grain, white, polished, dried, raw"


#├ New category from ID_2 ----

#Flour of sorghum, red

id2 <- "23120.06"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sorghum, red, flour raw"
dictionary.df[n1,13] <- "sorghum bicolor"

#Flour of millet, pearl

id2 <- "23120.05"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "millet, pearl, flour, raw"
dictionary.df[n1,12] <- "also called bulrush millet"
dictionary.df[n1,13] <- "pennisetum glaucum"

#Rice, brown, raw

id2 <- "23162"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "rice, brown, raw"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "oryza sativa"

#├ New item (ID_3) ----

#Adding description
dictionary.df$Description1[dictionary.df$ID_3 == "23710.01"] <- "default pasta (spaghetti)"

#Flour of sorghum, white

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23120.06.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23120.06.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sorghum, white, flour raw"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "sorghum bicolor"

#Flour of millet, finger

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23120.05.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23120.05.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "millet, finger, flour,raw"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "eleusine coracana"



n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23710.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23710.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pasta, macaroni, wheat, dried, raw"
dictionary.df[n1,10] <- NA
dictionary.df[n1,11] <- NA

## ├├ uncooked pasta, not stuffed or otherwise prepared (23710) -----

#Manual inputs:
food_desc <- c("pasta, macaroni, wheat, dried, raw",
               "Instant noodles, made from wheat flour, unfortified, without flavouring, unprepared")

other_name <- c("Indome (WA9)")

# Fixed
id2 <- "23710"
fex2 <- NA
scientific_name <- "ananas comosus"
desc1 <- "Fruit, nuts and peel, including frozen, prepared or preserved, jam, paste, marmalade, pure and cooked fruits, other than those listed separately (Unofficial definition)"
ref1 <- "https://www.fao.org/faostat/en/#data/SCL"


for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}



#rice, brown, boiled

id3 <- "23162.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,9] <- "rice, brown, boiled"

## ├├  rice, milled (husked) (23161.01) -----

food_desc <-  c("rice, parboiled, imported, raw",
                "rice, parboiled, imported, boiled",
                "rice, white, imported, steamed")

other_name <- c(rep(NA, 3))

scientific_name <- c(rep("oryza sativa", 3))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "23161.01"
desc1 <-  c("Rice, Milled (Husked): White rice milled from imported husked rice. Includes semi-milled, whole-milled and parboiled rice.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  rice, milled (23161.02) -----

food_desc <-  c("rice grain, parboiled, local, dried, raw",
                "rice grain, parboiled, local, dried, boiled",
                "rice grain, red native, milled, local, dried, raw", 
                "rice grain, white, local, steamed", 
                "rice grain, white, local, boiled", 
                "rice grain, white, polished, local, dried, boiled")

other_name <- c(rep(NA, 2), "African rice, milled",
                rep(NA, 3))

scientific_name <- c(rep("oryza sativa", 2), 
                     "oryza glaberrima", 
                     rep("oryza sativa", 3))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "23161.02"
desc1 <-  c("Rice, Milled: White rice milled from locally grown paddy. Includes semi-milled, whole-milled and parboiled rice.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  Bread (F0020) -----

food_desc <-  c("chapati, wheat flour, refined",
                "chapati, wheat flour, unrefined",
                "chapati, wheat flour, unrefined, with ghee",
                "chapati, wheat flour, refined, with fortified ghee",
                "bread, sweet, wheat flour, refined")

other_name <- c(rep(NA, 2),
                "also called \"Indian chapati\"",
                rep(NA, 2))

scientific_name <- c(rep(NA,  5))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "F0020"
desc1 <-  c("A baked product of flour or meal of cereals, especially wheat. Includes ordinary, unleavened, crackers, rusks, etc (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


#Add - 23120.01 - bread, sweet, wheat flour, refined
#Manual inputs:
id2 <- "23120.01"
desc_new <- "cake, rice flour, fried"
fex2_new <- NA
scien_new <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new


## ├├  other cereals n.e.c. (1199.9) -----

food_desc <-  c("teff grain, dried, unrefined, raw",
                "amaranth, grain, dried, raw" )

other_name <- c(rep(NA, 2))

scientific_name <- c("eragrostis tef", 
                     "amaranthus spp.")

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "1199.9"
desc1 <-  c("This subclass includes among all: Chenopodium pallidicaule (canagua or coaihua), Amaranthus caudatus (quihuicha or Inca wheat), Coix lacryma-jobi (adlay or Job's tears), Zizania aquatica (wild rice) and other cereal crops and hybrid grains that are not identified separately because of their minor relevance at the international level. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

# 114 - Sorghum, Grain, Red, Dried, Raw
#Manual inputs:
id2 <- "114"
desc_new <- "sorghum grain, red, dried, raw"
fex2_new <- NA
scien_new <- "sorghum bicolor"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 114 - Sorghum, Grain, white, Dried, Raw
#Manual inputs:
id2 <- "114"
desc_new <- "sorghum grain, white, dried, raw"
fex2_new <- NA
scien_new <- "sorghum bicolor"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23162 - Rice, brown, raw 
#Manual inputs:
id2 <- "23162"
desc_new <- "rice grain, brown, dried, raw"
fex2_new <- NA
scien_new <- "oryza sativa"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23162 - Rice, red native, hulled, raw 
#Manual inputs:
id2 <- "23162"
desc_new <- "rice grain, red native, brown, dried, raw"
fex2_new <- NA
scien_new <- "oryza glaberrima"
other_name <- "African rice, hulled"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├  pastry (F0022) -----

food_desc <-  c("dough, fried", "cake, plain butter", "cake, fruit",
                "cake, sponge, homemade", "cake, sponge, without fat", 
                "cake, iced", "croissant, plain, unfortified",
                "biscuit, savoury")

other_name <- c("mandazi (TZ06, KE18)", "cupcake", rep(NA,5))

scientific_name <- c(NA)
fex2_new <- c(NA)
id2 <- "F0022"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}



# 23999.02 - Cheese straws/twists, retail
#Manual inputs:
id2 <- "23999.02"
desc_new <- "snacks, cheese flavour"
fex2_new <- NA
scien_new <- NA
other_name <- "Cheese straws/twists (UK21)"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23999.02 -  Corn snacks
#Manual inputs:
id2 <- "23999.02"
desc_new <- "snacks, corn based"
fex2_new <- NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23999.02 -  Potato snacks, pringle-type, fried in vegetable oil 
#Manual inputs:
id2 <- "23999.02"
desc_new <- "snacks, potato based"
fex2_new <- NA
scien_new <- NA
other_name <- "pringle-type, fried potato snack (UK21)"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23120.03 -  Yellow maize flour
#Manual inputs:
id2 <- "23120.03"
desc_new <- "maize, flour, yellow, unrefined, non-fermented, raw"
fex2_new <- NA
scien_new <- "Zea mays L."
other_name <- "cornmeal"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 39120.06 -  Oat, bran, raw
#Manual inputs:
id2 <- "39120.06"
desc_new <- "oat, bran, raw"
fex2_new <- NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 39120.01 -  Wheat, bran, raw
#Manual inputs:
id2 <- "39120.01"
desc_new <- "wheat, bran, raw"
fex2_new <- NA
scien_new <- "triticum spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├  Wheat and meslin flour (23110) -----

food_desc <-  c("wheat flour, refined, fortified (GHA, SLE), dried, raw"
)

other_name <- c(rep(NA, 1))

scientific_name <- tolower(c("Triticum spp."))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "23110"
desc1 <-  c("Flour of Wheat: Defined broadly to include meal, groats and pellets. Strong flours from hard wheat are used for bread, while durum wheat flour is used primarily for pasta. Weaker flours from soft wheat are mainly used in cakes, pastries, biscuits and certain noodle")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  malt extract (23999.01) -----

food_desc <-  c("ovaltine, fortified, powder"
)

other_name <- c(rep(NA, 1))

scientific_name <- c(rep(NA, 1))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "23999.01"
desc1 <-  c("A concentrated solution from water-macerated malt, which is used in infant and dietetic foods and in bread baking (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  maize (corn) (112) -----

food_desc <-  c("popcorn, air popped")

other_name <- c(rep(NA, 1))

scientific_name <- tolower(c(rep("Zea mays", 1)))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "112"
desc1 <-  c("Maize (corn) This class includes: - maize harvested for their dry grains only")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  breakfast cereals (23140.03) -----

food_desc <-  c("Weet-Bix, breakfast cereal, unfortified")

other_name <- c("wheat biscuits (UK)")

scientific_name <- tolower(c(rep(NA, 1)))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "23140.03"
desc1 <-  c("Foods prepared by swelling and roasting cereals or cereal products, e.g. corn flakes, puffed rice; cereals o/t maize, in grain form, precooked or otherwise prepared (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  millet (118) -----

food_desc <-  c("pearl millet grain, dried, refined, raw")

other_name <- c(NA)

scientific_name <- tolower(c(rep(NA, 1)))

taxon <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "118"
desc1 <-  c(NA)
ref1 <-  c(NA)
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


### Animal products (AP) ----
##NOTES:
##Any "whole fat would be under raw milk

#Fixing codes
#Chicken prep.
dictionary.df$ID_2[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"] <- "F1061"
dictionary.df$FoodName_2[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"] <- "poultry meat preparations"
dictionary.df$ID_3[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"]  <- "F1061.02"

#Amend chicken stew (21121.03)  --> F1061.01
dictionary.df$ID_3[dictionary.df$ID_3 == "21121.03"] <- "F1061.01"
dictionary.df[which(dictionary.df$ID_3 == "F1061.01"), c(1:6)] <- unique(subset(dictionary.df, ID_2 == "F1061", select = c(1:6)))
#Amend goat meat, fresh, grilled  (21116.02)  --> F1172.02
dictionary.df$ID_3[dictionary.df$ID_3 == "21116.02"] <- "F1172.02"
dictionary.df[which(dictionary.df$ID_3 == "F1172.02"), c(1:6)] <- unique(subset(dictionary.df, ID_2 == "F1172", select = c(1:6)))


#beef prep.
dictionary.df$ID_2[dictionary.df$FoodName_3 == "beef, fresh, grilled"] <- "F0875"
dictionary.df$ID_3[dictionary.df$FoodName_3 == "beef, fresh, grilled"]  <- "F0875.01"

#Fixing food desc
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21111.01.01"] <- "beef, lean, with bones, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21111.02.01"] <- "beef, lean, without bones, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1501.02"] <- "North African catfish, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21170.01.03"] <- "quail meat, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21116.01"] <- "goat meat, fresh, average fat, raw"

#Adding scientific name
dictionary.df$scientific_name[dictionary.df$ID_3 == "1501.02"] <- "clarias gariepinus"

#Fixing FoodName_2
#of fish
dictionary.df$FoodName_2[dictionary.df$ID_2 == "1503"] <- "freshwater & diadromous fish, fresh fillets"
#of beef prep.
dictionary.df$FoodName_2[dictionary.df$ID_2 == "F0875"] <- "beef and veal preparations nes"

#├ New category from ID_2 ----

#Ice cream - loop

foods <- tolower(c("Ice cream, caramel flavour, regular fat" ,
                   "Ice cream, chocolate flavour, regular fat" ,
                   "Ice cream, strawberry flavour, regular fat",
                   "Ice cream, vanilla flavour, regular fat" ,           
                   "Ice cream, vanilla flavour, with nuts, regular fat"))

for(i in 1:length(foods)){
  
  id2 <- "22270"
  
  n1 <- dim(dictionary.df)[1]+i
  
  n2 <- which(dictionary.df$ID_2 %in% id2)
  
  dictionary.df[n1,] <- dictionary.df[n2,]
  
  dictionary.df[n1,7] <- paste0(id2, ".0", i)
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(foods[i])
  
}



#Pork sausages

id2 <- "21184.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pork, sausage, raw"
dictionary.df[n1,13] <- "sus scrofa domesticus"

# 21181 - Bacon rashers (pig meat)
#Manual inputs:
id2 <- "21181"
desc_new <- "bacon rashers"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- "bacon (pig meat) stripes"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Skimmed milk

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "22110.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "22110.02.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk, cow, skimmed, raw"
dictionary.df[n1,13] <- "bos taurus"


##  Fish -----

##├├ Pelagic fish, fillet, fresh, raw (1529) ----

fish_name <- tolower(c("Barracudas nei", "Seerfishes nei", "Sardinellas nei",
                       "True tunas nei", 
                       "atlantic horse mackerel",
                       "common dolphinfish", "european anchovy"))

scientific_name <- tolower(c("Sphyraena spp.", "Scomberomorus spp.", 
                             "Sardinella spp." , "Thunnus spp.", 
                           "trachurus trachurus",
                           "coryphaena hippurus", "engraulis encrasicolus"))

other_name <- c("Barracuda", "Mackerel", "Sardines, 
                Dagaa", "Tuna", NA, "mahi mahi (WA19)", "anchovy" )


isscaap <- c( "37", "36", "35", "36", "37", "37", NA)

taxo <- c("17710001XX", "17501015XX", "12105012XX",
          "17501026XX", NA, NA)

alpha <- c("BAR", "KGX", "SIX", "TUS", NA, NA, NA)

fex2_new <- c(rep(NA, 7))

#Manual inputs:
id2 <- "1529"
ref1 <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
ref2 <- NA

#Auto inputs:
for(i in 1:length(fish_name)){
  
  desc_new <- paste0(fish_name[i], ", fillet, fresh, raw") 
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- ref2
  
}



#Fish loop 
#we added several fishes from the same category
#Info of the English names, and other variables were taken 
#from the ASFIS dataset and from the Fisheries Global NCT
#work
#https://www.fao.org/fishery/en/collection/asfis/en

## ├├  Freshwater & diadromous fish, fresh fillets (1503) -----

food_desc <- tolower(c("West African lungfish", "Nile perch" ,"Nile tilapia" ,        
                       "Rhinofishes nei","Naked catfishes","Upsidedown catfishes" ,
                       "North African catfish" ,"Tilapias nei", 
                       "Carp", "Mormyrid"))

isscaap <- c( "13", "13" ,"12" ,"11", "13", "13" ,"13" ,"12", "11", "13")

taxo <- c("1160200202" ,"1700116707", "1705905102" ,"14002024XX", "14108111XX",
          "14132008XX","1411803003" ,"17059051XX", NA, NA)

alpha <- c("PPG" ,"NIP" ,"TLN" ,"RHI" ,"CAN", "CSY", "CLZ", "TLP", "FCP",
           "MZU")

scientific_name <-  tolower(c("Protopterus annectens", "Lates niloticus" ,
                              "Oreochromis niloticus", "Labeo spp",   "Bagrus spp"  ,
                              "Synodontis spp", "Clarias gariepinus" ,
                              "Oreochromis spp", "Cyprinus carpio",
                              "Mormyrus spp."))

other_name <- c(rep(NA, 9), "elephantfish or beaked fishes")


#Fixed
id2 <- "1503"
ref_taxon <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(food_desc[i], ", fillet, fresh, raw")
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- ref_taxon
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- NA
}


#├ New item (ID_3) ----

##├├  Fish -----

# Dried fish - general (00)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1505.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1505.00.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "fish, dried, raw"
dictionary.df[n1,10] <- "multiple species, used as \"mean\", \"other\""
dictionary.df[n1,11] <- NA
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- NA

#Sardines, grilled

id3 <- "1533.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(str_extract(id3, 
                                          "[[:alnum:]]{2,5}\\.\\d{1}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                              as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sardines, fresh, grilled"
dictionary.df[n1,10] <- "ISSCAAP Code:35; Taxonomic Code:12105012XX; Inter-Agency3-Alpha Code:SIX"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "sardinella spp."
dictionary.df[n1,12] <- "10.48580/dfpk-37v"

# Nile perc, dried

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1505.06")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1505.08"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "nile perc, dried, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:13; Taxonomic Code:1700116707; Inter-Agency3-Alpha Code:NIP"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "lates niloticus"

# Rhinofishes nei (sometimes called catfish)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1501.06")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1501.07"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "rhinofishes nei, fresh, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:11; Taxonomic Code:14002024XX; Inter-Agency3-Alpha Code:RHI"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- "also called catfish, african carp"
dictionary.df[n1,13] <- "labeo spp."

# Naked catfishes (sometimes called catfish)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1501.07")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1501.08"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "naked catfishes, fresh, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:13; Taxonomic Code:14108111XX; Inter-Agency3-Alpha Code:CAN"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- "also called catfish, bayad"
dictionary.df[n1,13] <- "bagrus spp."


# Shark

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1514.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1514.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "shark, fresh, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:38; Taxonomic Code:10802010XX; Inter-Agency3-Alpha Code:CWZ"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- NA
dictionary.df[n1,13] <- "carcharhinus spp."

#seerfishes nei, fillet, fresh, raw

id3 <- "1533.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,9] <- "seerfishes nei, fillet, fresh, grilled"
dictionary.df[n1,10] <- "ISSCAAP Code: 36; Taxonomic Code: 17501015XX; Inter-Agency3-Alpha Code: KGX"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- "also called Mackerel"
dictionary.df[n1,13] <- "scomberomorus spp."


#Shrimp (crayfish), whole, dried
#Manual inputs:
id2 <- "1555"
desc_new <- "shrimp, whole, dried, raw"
fex2_new <- NA
taxon <- "ISSCAAP Code: 45; Taxonomic Code: NA; Inter-Agency3-Alpha Code: PAL"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "Families: Palaemonidae/Penaeidae"
other_name <- "crayfish"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 Cod, fillet, steamed (without salt)
#Manual inputs:
id2 <- "1520"
desc_new <- "Cod, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 32; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "gadus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 Cod, fillet, grilled (without salt and fat)
#Manual inputs:
id2 <- "1520"
desc_new <- "Cod, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 32; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "gadus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 -  Cod, fillet, boiled in recipe (without salt and fat)
#Manual inputs:
id2 <- "1520"
desc_new <- "Cod, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 32; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "gadus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 -  Shark, steamed (without salt)
#Manual inputs:
id2 <- "1520"
desc_new <- "shark, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 38; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "carcharhinus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 -  Shark, grilled (without salt and fat)
#Manual inputs:
id2 <- "1520"
desc_new <- "shark, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 38; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "carcharhinus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1520 -   Shark, boiled in recipe (without salt and fat)
#Manual inputs:
id2 <- "1520"
desc_new <- "shark, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 38; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "carcharhinus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Dagaa fish (omena), dried, stewed (without salt)
#Manual inputs:
id2 <- "1507"
desc_new <- "dagaa fish, dried, stewed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 11; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "rastrineobola argentea"
other_name <- "omena"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Eel, steamed (without salt)
#Manual inputs:
id2 <- "1507"
desc_new <- "eel, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 22; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "anguilla spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Eel, grilled (without salt and fat)
#Manual inputs:
id2 <- "1507"
desc_new <- "eel, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 22; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "anguilla spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Eel, boiled in recipe (without salt and fat)
#Manual inputs:
id2 <- "1507"
desc_new <- "eel, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 22; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "anguilla spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Herring stock, steamed (without salt)
#Manual inputs:
id2 <- "1507"
desc_new <- "herring stock, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 24; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "ilisha melastoma"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Herring stock, grilled (without salt)
#Manual inputs:
id2 <- "1507"
desc_new <- "herring stock, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 24; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "ilisha melastoma"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Herring stock, boiled in recipe (without salt and fat) 
#Manual inputs:
id2 <- "1507"
desc_new <- "herring stock, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 24; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "ilisha melastoma"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


# 1507 -  Lungfish, fillet, steamed (without salt) 
#Manual inputs:
id2 <- "1507"
desc_new <- "lungfish, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 13; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "protopterus annectens"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Lungfish, fillet, grilled (without salt and fat) 
#Manual inputs:
id2 <- "1507"
desc_new <- "lungfish, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 13; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "protopterus annectens"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 1507 -  Lungfish, fillet, boiled in recipe (without salt and fa~
#Manual inputs:
id2 <- "1507"
desc_new <- "lungfish, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 13; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "protopterus annectens"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├  Freshwater fish, preparation ns (1507) -----

food_desc <-  c("north african catfish, fresh, steamed",
                "north african catfish, fresh, grilled", 
                "north african catfish, fresh, boiled", 
                "north african catfish, fresh, fried", 
                "mochokid catfish, fresh, grilled", 
                "mola carplet")

scientific_name <-  tolower(c(rep("clarias gariepinus", 4), 
                      "Synodontis spp.",
                      "Amblypharyngodon mola"))

other_name <-  c(rep("mudfish (kamongo) (KE18)", 3), 
                 "Mlamba wokazinga (MW19)", 
                 "catfish (WA19)", 
                  NA)

isscaap <- c(rep("13",5), "11")

taxo <- c(rep(NA, 6))

alpha <- c(rep("CLZ",4), "CSY", "ABO")

ref2 <- c(rep("https://www.fishbase.se/ComNames/CommonNameSummary.php?autoctr=53372", 3),
          NA)

#Fixed
id2 <- "1507"
ref_taxon <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"


# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- ref_taxon
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- ref2[i]
}


# 1557 -  Prawns, fresh, steamed (without salt)
#Manual inputs:
id2 <- "1557"
desc_new <- "prawns, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code: 45; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "Penaeidae"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1557 -  Prawns, fresh, boiled in recipe (without salt and fat)
#Manual inputs:
id2 <- "1557"
desc_new <- "prawns, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code: 45; Taxonomic Code: NA; Inter-Agency3-Alpha Code: NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "Penaeidae"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1533 - Sardine, fresh, steamed (without salt)
#Manual inputs:
id2 <- "1533"
desc_new <- "sardines, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code:35; Taxonomic Code:12105012XX; Inter-Agency3-Alpha Code:SIX"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "sardinella spp."
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1533 - Sardine, fresh, boiled in recipe (without salt and fat)
#Manual inputs:
id2 <- "1533"
desc_new <- "sardines, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code:35; Taxonomic Code:12105012XX; Inter-Agency3-Alpha Code:SIX"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "sardinella spp."
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1533 - Tuna, steamed (without salt) 
#Manual inputs:
id2 <- "1533"
desc_new <- "tuna, fresh, steamed"
fex2_new <- NA
taxon <- "ISSCAAP Code:36; Taxonomic Code:NA; Inter-Agency3-Alpha Code:NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "thunnus albacares/t. thynnus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1533 - Tuna, grilled (without salt and fat)
#Manual inputs:
id2 <- "1533"
desc_new <- "tuna, fresh, grilled"
fex2_new <- NA
taxon <- "ISSCAAP Code:36; Taxonomic Code:NA; Inter-Agency3-Alpha Code:NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "thunnus albacares/t. thynnus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1533 - Tuna, boiled in recipe (without salt and fat)  
#Manual inputs:
id2 <- "1533"
desc_new <- "tuna, fresh, boiled"
fex2_new <- NA
taxon <- "ISSCAAP Code:36; Taxonomic Code:NA; Inter-Agency3-Alpha Code:NA"
ref1 <-  "See Fisheries Global NCT, 2022, https://www.fao.org/fishery/en/collection/asfis/en"
scien_new <- "thunnus albacares/t. thynnus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

#Sardines
#Manual inputs:
id2 <- "1527"
desc_new <- "sardines, fresh, raw"
fex2_new <- NA
taxon <- "ISSCAAP Code:35; Taxonomic Code:12105012XX; Inter-Agency3-Alpha Code:SIX"
ref1 <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
scien_new <- "sardinella spp."
other_name <- NA
ref2 <- "10.48580/dfpk-37v"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1527 - Anchovy, fillet, raw   
#Manual inputs:
id2 <- "1527"
desc_new <- "european anchovy, fresh, raw"
fex2_new <- NA
taxon <- "ISSCAAP Code:35; Taxonomic Code:1210600201; Inter-Agency3-Alpha Code:ANE"
ref1 <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
scien_new <- "engraulis encrasicolus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2



## ├├  Freshwater fish, fresh (1501) -----

food_desc <-  c("upsidedown catfishes, fresh, raw",
  "indian ilisha, fresh, raw", 
  "mola carplet, fresh, raw")

scientific_name <-  c("synodontis spp.",
  "ilisha melastoma", 
  "Amblypharyngodon mola")

other_name <-  c("also called catfish",
  "herring stock, raw (KE18)")

taxon <- c("ISSCAAP Code:24; Taxonomic Code:1211200102; Inter-Agency3-Alpha Code:PIE",
         "ISSCAAP Code:13; Taxonomic Code:14132008XX; Inter-Agency3-Alpha Code:CSY",
         NA)

#Fixed
id2 <- "1501"
ref_taxon <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"


# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- taxon[i]
  dictionary.df[n1,11] <- ref_taxon
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


# 1533 - pelagic fish, fillet, grilled 
fish_name <-  c("atlantic horse mackerel",
                "Barracuda",
                "common dolphinfish")
scientific_name <-  c("trachurus trachurus",
                      "sphyraena spp.", 
                      "coryphaena hippurus")
other_name <-  c(NA, "mahi mahi (WA19")

for(i in 1:length(fish_name)){
  #Manual inputs:
  id2 <- "1533"
  desc_new <- paste0(fish_name[i], ", fillet, grilled") 
  fex2_new <- NA
  taxon <- "ISSCAAP Code:37; Taxonomic Code:NA; Inter-Agency3-Alpha Code:NA"
  ref1 <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
  scien_new <- scientific_name[i]
  other_name <- other_name[i]
  ref2 <- NA
  
  #Auto inputs:
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,10] <- taxon
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name
  dictionary.df[n1,13] <- scien_new
  dictionary.df[n1,14] <- ref2
  
}

# 1520 - fish balls
#Manual inputs:
id2 <- "1520"
desc_new <- "fish balls, steamed"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1516 - Shiny-nose, fillet, raw (WA19)
#Manual inputs:
id2 <- "1516"
desc_new <- "shiny nose, fillet, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "polydactylus spp."
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1516 - Flathead grey mullet (WA19)
#Manual inputs:
id2 <- "1516"
desc_new <- "flathead grey mullet, fillet, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "mugil cephalus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1520 - Flathead grey mullet, grilled (WA19)
#Manual inputs:
id2 <- "1520"
desc_new <- "flathead grey mullet, fillet, fresh, grilled"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "mugil cephalus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1520 - Shiny-nose, fillet, grilled (WA19)
#Manual inputs:
id2 <- "1520"
desc_new <- "shiny nose, fillet, fresh, grilled"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "polydactylus spp."
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1520 - White grouper, fillet, grilled (WA19)
#Manual inputs:
id2 <- "1520"
desc_new <- "white grouper, fillet, fresh, grilled"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "epinephelus spp."
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1520 - croaker, baked (JA15)
#Manual inputs:
id2 <- "1520"
desc_new <- "croaker, fillet, fresh, baked"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "pseudotolithus spp"
other_name <- NA
ref2 <- "No ref. but assumption acc. to (SL18)"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

# 1516 - croaker, fillet, raw (JA15)
#Manual inputs:
id2 <- "1516"
desc_new <- "croaker, fillet, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "pseudotolithus spp"
other_name <- NA
ref2 <- "No ref. but assumption acc. to (SL18)"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2


## ├├  Demersal fish, fresh fillets (1516) -----

food_desc <-  c("Atlantic cod, fillet, fresh, raw"
                )

scientific_name <-  c("gadus morhua"
                      )

other_name <-  c("Northeast Atlantic cod (WA19)"
                    )

taxon <- c("ISSCAAP Code:32; Taxonomic Code:NA; Inter-Agency3-Alpha Code:COD"
      )

#Fixed
id2 <- "1516"
ref_taxon <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"


# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- taxon[i]
  dictionary.df[n1,11] <- ref_taxon
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}




# 1514 - Atlantic croaker, raw (JA15)
#Manual inputs:
id2 <- "1514"
desc_new <- "Atlantic croaker, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "micropogonias undulatus"
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2


## ├├  Pelagic fish, canned (1532) -----

food_desc <-  tolower(c("tuna, canned in water, drained", 
                "tuna, canned in oil, drained", 
                "Mackerel, jack, canned in oil, drained"))

scientific_name <- c(rep("Thunnus spp.", 2), 
                     "Trachurus symmetricus")

other_name <- c("tuna, tinned in water, drained", 
                "tuna, tinned in oil, drained",
                "Mackerel, jack, tinned in oil, drained")

fex2_new <- c(rep(NA, 2))

#Fixed
id2 <- "1532"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ Demersal fish, body oil (1522) -----

food_desc <-  c("salmon, body, oil")

scientific_name <- c(NA)

other_name <- c("")

fex2_new <- c(NA)

#Fixed
id2 <- "1522"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├  Pelagic fish, cured (1531) -----

food_desc <-  c("mackerel, fillet, salted, plain, raw",
                "herring, Pacific, smoked, raw")

scientific_name <- c(rep(NA, 2))

other_name <- c("mackerel, \"Shiosaba\" plain salted fillet (JA15)",
                NA)

fex2_new <-  c(rep(NA, 2))

#Fixed
id2 <- "1531"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├  Molluscs, fresh (1562) -----

food_desc <-  c("clams, venus, fresh, raw",
                "mussels, fresh, raw", 
                "cockles, fresh, raw", 
                "sea snail, fresh, raw")

scientific_name <- c("veneridae spp.", rep(NA, 2), 
                     "Buccinidae spp.")

other_name <- c(rep(NA, 3), "whelk (WA19)")

fex2_new <- c(rep(NA, 4))

#Fixed
id2 <- "1562"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

############ End fish ##################

# 22221.01 - Milk, cow, canned, evaporated
#Manual inputs:
id2 <- "22221.01"
desc_new <- "milk, cow, canned, evaporated"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

#Beef, high fat w/o bones

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "21111.02.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21111.02.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "beef, high fat, without bones, fresh, raw"
dictionary.df[n1,13] <- "bos taurus"

#Beef, moderate fat w/o bones

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "21111.02.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21111.02.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "beef, moderate fat, without bones, fresh, raw"
dictionary.df[n1,13] <- "bos taurus"

#Beef, high fat w/ bones

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "21111.01.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21111.01.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "beef, high fat, with bones, fresh, raw"
dictionary.df[n1,13] <- "bos taurus"

#Beef, moderate fat w/ bones

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "21111.01.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21111.01.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "beef, moderate fat, with bones, fresh, raw"
dictionary.df[n1,13] <- "bos taurus"


## ├├  Crustaceans, fresh (1553) -----

food_desc <- tolower(c("freshwater prawns", "marine shrimp" ))

isscaap <- c("45", "45" )

taxo <- c(NA, NA)

alpha <- c("PAL", "PAL" )

scientific_name <-  tolower(c("Family: Palaemonidae",
                              "Family: Penaeidae"))

other_name <- c("Shrimp, palaemonid (WA19)", 
                "Shrimp, penaeid (WA19)")


#Fixed
id2 <- "1553"
ref_taxon <-  "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(food_desc[i], ", fresh, raw")
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- ref_taxon
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- NA
}


## ├├ Cream, fresh (22120) -----

food_desc <-  c("cream, cow milk, 35%", 
                "cream, cow milk, reduced fat, 18%")

scientific_name <- tolower(c(rep("Bos taurus", 2)))

other_name <- c(rep(NA, 2))

fex2_new <- c(rep(NA, 2))

#Fixed
id2 <- "22120"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├meat of chickens, fresh or chilled (21121) -----

food_desc <-  c("chicken, dark meat, without skin, without bones, fresh, raw", 
                "chicken, dark meat, with skin, without bones, fresh, raw", 
                "chicken, light meat, with skin, without bones, fresh, raw",
                "chicken, light meat, without skin, without bones, fresh, raw")

scientific_name <- tolower(c(rep("Gallus gallus domesticus", 4)))

other_name <- c(rep(NA, 2))

fex2_new <- c(rep(NA, 2))

#Fixed
id2 <- "21121"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


#Silver cyprinid
subset(dictionary.df, ID_2 == "1505")

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1505.03")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1505.07"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cyprinid, silver, dried, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:11; Taxonomic Code:1400207001; Inter-Agency3-Alpha Code:ENA"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/07/19). (2022)"
dictionary.df[n1,12] <- "Lake Victoria sardine (https://www.catalogueoflife.org/data/taxon/4RLTW)"
dictionary.df[n1,13] <- "rastrineobola argentea"


#milk flavoured, chocolate 

id3 <- "22290.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk flavoured, chocolate"

#milk flavoured, strawberry, banana

id3 <- "22290.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk flavoured, strawberry, banana"

#milk fermented, industrial

id3 <- "22290.03"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk fermented, industrial"

#milk fermented, traditional

id3 <- "22290.04"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk fermented, traditional"


#Add - F0875 - meatballs, beef 
subset(dictionary.df, str_detect(FoodName_2, "beef"))
subset(dictionary.df, ID_2 == "F0875")

#Manual inputs:
id2 <- "F0875"
desc_new <- "meatballs, beef, cooked"
fex2_new <- NA
scien_new <- "bos taurus"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-paste0( str_extract(id3, 
                              "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                  as.numeric(str_extract(id3, "[[:digit:]]$"))+1)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# 21122 - Duck, meat and skin, fresh, raw
#Manual inputs:
id2 <- "21122"
desc_new <- "duck, meat and skin, fresh, raw"
fex2_new <- NA
scien_new <- "anas platyrayncha"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21156 - Goat liver, raw
#Manual inputs:
id2 <- "21156"
desc_new <- "goat liver, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "capra aegagrus hircus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21155 - Lamb liver, raw
#Manual inputs:
id2 <- "21155"
desc_new <- "lamb liver, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "ovis aries"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21155 - Lamb brain, raw
#Manual inputs:
id2 <- "21155"
desc_new <- "lamb brain, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "ovis aries"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21151 -  Beef kidney, raw
#Manual inputs:
id2 <- "21155"
desc_new <- "beef kidney, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "bos taurus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21151 -  Beef tripe, raw
#Manual inputs:
id2 <- "21155"
desc_new <- "beef tripe, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "bos taurus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21160.01 -  Chicken giblets, raw
#Manual inputs:
id2 <- "21160.01"
desc_new <- "chicken giblets, fresh,  raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "gallus gallus domesticus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21160.01 -  Chicken liver, raw
#Manual inputs:
id2 <- "21160.01"
desc_new <- "chicken liver, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "gallus gallus domesticus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21182 -  beef meat, lean, cured, dried
#Manual inputs:
id2 <- "21182"
desc_new <- "beef meat, lean, cured, dried, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "bos taurus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

## ├├ Other meat and edible meat offal, salted, ... (21183) -----
## in brine, dried or smoked; edible flours and meals of meat or meat offal

# Manual inputs:
food_desc <- c("game meat, dried, salted, raw",
               "termites, dried, raw", 
               "caterpillar, dried, roasted",
               "cricket, dried, raw")

scientific_name <- tolower(c(NA, 
                     "Macrotermes spp.",
                     "Usta terphrichore",
                     NA))

other_name <- c(NA, 
                "Ngumbi (Macrotermes subhyanlinus) (MW19)", 
                "Nyamanyama zootcha (MW19)",
                NA)

# 
id2 <- "21183"
fex2_new <- c(NA)
desc1 <-  c(NA)
ref1 <-  c(NA)


# Function: 
for(i in 1:length(food_desc)){
  
  #id2 <- id2
  # desc_new <- food_desc[i]
  # fex2_new <- fex2_new[i]
  #  scien_new <- scientific_name[i]
  #desc1 <- desc1[i]
  #  ref1 <- ref1
  #   other_name <- other_name
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1[i]
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  
}



# 21116 -  Goat, lean, fresh, raw
#Manual inputs:
id2 <- "21116"
desc_new <- "goat, lean, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "capra aegagrus hircus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 21116 -  Goat, lean, fresh, raw
#Manual inputs:
id2 <- "21116"
desc_new <- "goat, medium fat, fresh, raw"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "capra aegagrus hircus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# F1061 - chicken feet, boiled
#Manual inputs:
id2 <- "F1061"
desc_new <- "chicken feet, fresh, boiled"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#22270 - Ice cream, generic flavour (including lollies)
#Manual inputs:
id2 <- "22270"
desc_new <- "ice cream, generic flavour (including lollies)"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#2293 - Camel milk, fresh
#Manual inputs:
id2 <- "2293"
desc_new <- "camel milk, fresh"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "camelus dromedarius"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#2292 - Goat milk, fresh
#Manual inputs:
id2 <- "2292"
desc_new <- "goat milk, fresh"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "capra aegagrus hircus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#2291 - Sheep milk, fresh
#Manual inputs:
id2 <- "2291"
desc_new <- "sheep milk, fresh"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- "ovis aries"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#22241.01 - Butter, from cow’s milk, salted
#Manual inputs:
id2 <- "22241.01"
desc_new <- "butter, cow's milk, salted"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#22241.01 - Butter, from cow’s milk, unsalted
#Manual inputs:
id2 <- "22241.01"
desc_new <- "butter, cow's milk, unsalted"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#22222.01 - Milk, cow, canned, condensed, whole, sweetened
#Manual inputs:
id2 <- "22222.01"
desc_new <- "milk, cow, canned, condensed, whole, sweetened"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#22222.02 - Milk, cow, canned, condensed, skimmed, sweetened
#Manual inputs:
id2 <- "22222.02"
desc_new <- "milk, cow, canned, condensed, skimmed, sweetened"
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


#22290 - Malted & choco milk
food_desc <- c(                         
                  "chocolate malted milk beverage, skimmed" ,          
                  "chocolate milk beverage, whole, from powder",
                  "chocolate milk beverage, whole from syrup",                      
                  "malted milk, whole, from powder", 
                  "chocolate milk and whey beverage") 

for(i in 1:length(food_desc)){
#Manual inputs:
id2 <- "22290"
desc_new <- food_desc[i]
fex2_new <- NA
taxon <- NA
ref1 <-  NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
}



## ├├ Animal fat prep (F1243.01) -----

# Manual inputs:
food_desc <- c("animal, fat")

scientific_name <- c(NA)

id2 <- "F1243"
fex2_new <- c(NA)
desc1 <-  c(NA)
ref1 <-  c(NA)
other_name < -c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  #id2 <- id2
  # desc_new <- food_desc[i]
  # fex2_new <- fex2_new[i]
  #  scien_new <- scientific_name[i]
  #desc1 <- desc1[i]
  #  ref1 <- ref1
  #   other_name <- other_name
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1[i]
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├ Beef sausages and similar (21184.01) -----

#Manual inputs:
food_desc <- c("beef, sausage, raw",
               "beef, meat, ground, raw")

scientific_name <- c("bos taurus", 
                     "bos taurus")


for(i in 1:length(food_desc)){
  
  id2 <- "21184.01"
  desc_new <- food_desc[i]
  fex2_new <- NA
  scien_new <- scientific_name[i]
  desc1 <- "Preparations of meat or offal, whether chopped, minced or of blood.They may be raw, cooked or smoked and contain other ingredients, andare then enclosed in natural or artificial casings. (Unofficial definition)"
  ref1 <- NA
  other_name <-NA
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name
  dictionary.df[n1,13] <- scien_new
}


## ├├  Other meats (21170.92) ----
#Manual inputs:

food_desc <- c( "crocodile meat", 
                  "ostrich meat", 
                "springbok meat", 
                "grasshoppers, brown",
                "grasshoppers, green",
                "termites",
                "cricket", 
                "cricket, bush")

scientific_name <- tolower(c("crocodylus  spp.",
  "struthio camelus", 
                     "antidorcas marsupialis", 
                     "ruspolia baileyi",
                     "ruspolia baileyi",
                      NA, NA,
  "Ruspolia differens"))

other_name <- c(rep(NA, 9), "katydid (WA19)")

for(i in 1:length(food_desc)){
  
  id2 <- "21170.92"
  desc_new <- paste0(food_desc[i], ", fresh, raw")
  fex2_new <- NA
  taxon <- NA
  ref1 <-  NA

  
  #Auto inputs:
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,10] <- taxon
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ Cheese (22251.01) -----

#Manual inputs:
food_desc <- c("cheese, cow's milk, cheddar",
          "cheese, cow's milk, skimmed, cottage",
          "cheese, cow's milk, whole, cottage", 
          "cheese, cow's milk, fresh curd", 
          "cheese, cow's milk, gouda", 
          "cheese, cow's milk, spreadable")

for(i in 1:length(food_desc)){
  
  id2 <- "22251.01"
  desc_new <- food_desc[i]
  fex2_new <- NA
  scien_new <- NA
  taxo <- NA
  ref1 <- NA
  other_name <-NA
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,10] <- taxon
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name
  dictionary.df[n1,13] <- scien_new
}


## ├├  Meat of pig boneless, fresh or chilled (21113.02) -----

food_desc <-  c("pork meat, minced, fresh, raw", 
                "pork meat, without bones, fatty (40%), fresh, raw", 
                "pork meat, without bones, lean (10%), fresh, raw")

other_name <- c("pig meat, ground, fresh, raw", 
                "pig meat, boneless, 40% fat",
                "pig meat, boneless, 20% fat")

scientific_name <- tolower(rep("sus scrofa domesticus", 3)) 

fex2_new <- c(NA)

#
id2 <- "21113.02"
desc1 <-  c("Pig meat, excluding butcher fat and bones, fresh or chilled (Unofficial definition)")
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├  Yoghurt (22230.01) -----

food_desc <-  c("yoghurt, cow milk, skimmed, plain")
other_name <- c(NA)

scientific_name <- c(NA)
fex2_new <- c(NA)
id2 <- "22230.01"
desc1 <-  c("A fermented milk food (Unofficial definition)")
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├  yoghurt, with additives (22230.02) -----

food_desc <-  c("yoghurt, cow milk, skimmed, sweetened")
other_name <- c(NA)

scientific_name <- c(NA)
fex2_new <- c(NA)
id2 <- "22230.02"
desc1 <-  c("Includes additives such as sugar, flavouring materials, fruit or coccoa (Unofficial definition)")
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

# 

## ├├  cheese from milk of goats, fresh or processed (22254) -----

food_desc <-  c("cheese, from goat’s milk, hard type")
other_name <- c(NA)

scientific_name <- c(NA)
fex2_new <- c(NA)
id2 <- "22254"
desc1 <-  c("Includes additives such as sugar, flavouring materials, fruit or coccoa (Unofficial definition)")
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ Eggs from other birds in shell, fresh, n.e.c. (232) -----

food_desc <-  c("egg, duck, fresh, raw", 
                "egg, guinea fowl, fresh, raw" , 
                "egg, quail, fresh, fresh, raw", 
                "egg, turkey, fresh, fresh, raw")

scientific_name <- c("anas platyrhynchos", "numida meleagris",
                     "coturnix japonica", "meleagris gallopavo")

other_name <- c(rep(NA, 4))

fex2_new <- c(rep(NA, 4))

#Fixed
id2 <- "232"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├ hen eggs in shell, fresh (231) -----

food_desc <-  c("egg, chicken, local breed, fresh, raw")

scientific_name <- tolower(c("Gallus gallus domesticus"))

other_name <- c(rep(NA, 4))

fex2_new <- c(rep(NA, 4))

#Fixed
id2 <- "231"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ snails, fresh, chilled, frozen, dried, salted or in brine (2920) -----
# except sea snails 

food_desc <-  c("snail, fresh, raw")

scientific_name <- c("achatina spp.")

other_name <- c(rep(NA, 1))

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "2920"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├ meat of camels, fresh or chilled (21117.01) -----

food_desc <-  c("camel meat, fresh, raw")

scientific_name <- tolower(c("Camelus dromedarius"))
                             
other_name <- c(rep(NA, 1))

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "21117.01"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ meat of sheep, fresh or chilled (21115) -----

food_desc <-  c("sheep meat, medium fat (20% fat), fresh, raw")

scientific_name <- tolower(c("Ovis aries"))

other_name <- c("lamb meat, moderately fat (20% fat)(WA19)")

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "21115"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├ Meat, other and products (F1172) -----

food_desc <-  c("beef, corned, canned")

scientific_name <- tolower(c("Bos taurus"))

other_name <- c("corned beef, tinned")

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "F1172"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├ Meat of turkeys, fresh or chilled (21124) -----

food_desc <-  c("turkey, meat, average, fresh, raw")

scientific_name <- tolower(c("Meleagris gallopavo"))

other_name <- c(NA)

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "21124"
desc1 <-  c("Meat of turkeys, fresh or chilled This subclass includes: - meat of turkeys, birds of subclass 02152, fresh or chilled This subclass does not include: - meat of turkeys, frozen, cf. 21144 - edible offal of turkey, cf. 21160")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ buffalo fat, unrendered (21513) -----

food_desc <-  c("Beef, fat, imported, chuck, raw")

scientific_name <- tolower(c("Bos taurus"))

other_name <- c(NA)

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "21513"
desc1 <-  c("")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


### Pulses and Beans (PB) ----
#Fixing ID_1
#Old FBS groundnut to new FBS groundnut code
dictionary.df$ID_1[dictionary.df$ID_1 == "2556"] <- "2552"
dictionary.df$FoodName_1[dictionary.df$ID_1 == "2552"] <- "Groundnuts"
dictionary.df$Description1[dictionary.df$ID_1 == "2552"] <- "Default composition: 242 Groundnuts, with shell, 243 Groundnuts, shelled, 246 Groundnuts, prepared, 247 Peanut butter. NOTE: old FBS code 2556"

#Fixing names
#Peanuts 
dictionary.df$FoodName_3[dictionary.df$ID_3 == "142.01"] <- "peanuts, unshelled, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21421.01"] <- "peanuts, shelled, dried, raw"


#├ New category from ID_2 ----

#Peanut butter

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "21495.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21495.02.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "peanut butter"
dictionary.df[n1,13] <- "arachis hypogaea"

#├ New item (ID_3) ----

## ├├ broad beans and horse beans, dry (1702) -----

food_desc <-  c("horse bean, dried, raw", 
                "broad bean, dried, raw",
                "broad bean, not soaked, boiled, drained")

other_name <- c(rep(NA, 3))

fex2_new <- c(rep(NA, 3))

#Fixed
id2 <- "1702"
scientific_name <- "vicia faba"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}


## ├├ soya beans (141) -----

food_desc <-  c("soybeans, milk, fresh, raw", 
                "soybeans, not soaked, boiled, drained")

other_name <- c(rep(NA, 2))

fex2_new <- c(rep(NA, 2))

#Fixed
id2 <- "141"
scientific_name <- "glycine max"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}


#Manual inputs:
id2 <- "141"
desc_new <- "soybeans, milk, fresh, raw"
fex2_new <- NA
scien_new <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-paste0( str_extract(id3, 
                              "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                  as.numeric(str_extract(id3, "[[:digit:]]$"))+1)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

#Add 1701 lima beans, dried, raw
#Manual inputs:
id2 <- "1701"
desc_new <- "lima beans, dried, raw"
fex2_new <- NA
scien_new <- "phaseolus lunatus"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

#Add 1709.9 - Lablab, dried, raw
#Manual inputs:
id2 <- "1709.9"
desc_new <- "lablab, dried, raw"
fex2_new <- NA
scien_new <- "lablab purpureus"
taxon <- "5J7RR" # This is the taxon Id from COL (catalogue of life)
ref1 <-  "https://www.catalogueoflife.org/data/taxon/5J7RR"
other_name <- "hyacinth bean, bonavist bean (KE18)"
ref2 <- "https://www.rhs.org.uk/plants/59705/lablab-purpureus/details"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add 1709.9 - Gram, black, dry, raw 
#Manual inputs:
id2 <- "1709.9"
desc_new <- "gram, black, dried, raw"
fex2_new <- NA
scien_new <- "vigna mungo"
taxon <- "451211" # This is the taxon Id from COL (catalogue of life)
ref1 <-  "https://www.checklistbank.org/dataset/2304/taxon/451211"
other_name <- "Amberique, Urd, Urd Bean, Urid"
ref2 <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:525443-1/general-information"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxon
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

## ├├ pulses, other and products (1706) -----

food_desc <-  c("cow pea, black, dried, raw",
  "cow pea, brown, dried, raw",
  "cow pea, white, dried, raw", 
  "cow pea, black, not soaked, boiled, drained")

scientific_name <- c(rep("vigna unguiculata", 4))

other_name <- c(rep(NA, 4))

fex2_new <- c("A013N#F28.A07HS", "A013N#F28.A07HS", rep(NA, 2))

taxo <- c(rep("7FSHX", 2))

ref2 <- c(rep("https://www.checklistbank.org/dataset/9842/taxon/7FSHX", 2))

#Fixed
id2 <- "1706"
desc1 <-  c("Cow peas, dry This subclass is defined through the following headings/subheadings of the HS 2007: 0713.35.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxo[i]
  dictionary.df[n1,15] <- ref2[i]
}

 

### Fruits and Vegetables (FV) ----

#Leaves can be fresh or dried. 
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.01"] <- "spinach, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.02"] <- "amaranth leaves, fresh, raw"
#Cabbage, head (generic item .01), change from 1212.03.
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1212.01"] <- "cabbages, head, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1212.03"] <- "cabbages, white, fresh, raw"
#Adding scientific name 
dictionary.df$scientific_name[dictionary.df$ID_3 == "1359.9.01"] <- "ziziphus spp.."
dictionary.df$scientific_name[dictionary.df$ID_3 == "21397.01.01"] <- "agaricus spp."
dictionary.df$scientific_name[dictionary.df$ID_3 %in% c("1212.01",
                                                        "1212.03")] <- "brassica oleracea var. capitata"
#Adding info to FoodName_3 & scientific name of dates
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1314.01"] <- "dates, dried, raw"
dictionary.df$scientific_name[dictionary.df$ID_3 == "1314.01"] <- "phoenix dactylifera"

#├ New category from ID_2 ----

#Juices, canned bottled, sweetened

id2 <-  "21439.9"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "juices, canned bottled, sweetened"


#Broad beans, green

id2 <-  "1243"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "broad beans, green, raw"
dictionary.df[n1,13] <- "vicia faba"

#Capsicum, green

id2 <-  "1231"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "capsicum, green, raw"
dictionary.df[n1,13] <- "capsicum annuum"

#Strawberry

id2 <-  "1354"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "strawberries, fresh, raw"
dictionary.df[n1,13] <- "fragaria X ananassa"


#Peaches

id2 <-  "1345"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "peaches, fresh, raw"
dictionary.df[n1,13] <- "prunus persica"


#Cauliflower
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1213")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1213.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cauliflower, fresh, raw"
dictionary.df[n1,13] <- "brassica oleracea var. botrytis"

#Leeks
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1254")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1254.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "leeks, fresh, raw"
dictionary.df[n1,13] <- "allium porrum"

#Plums
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1346")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1346.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "plums, purple skin, fresh, raw"
dictionary.df[n1,13] <- "prunus domestica"

#Tomato tinned

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "21399.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "21399.02.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "tomatoes, whole, tinned"
dictionary.df[n1,13] <- NA


#Eggplants
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1233")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1233.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "eggplant, fresh, raw"
dictionary.df[n1,13] <- "solalum melongena"

#Spring onions
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1253.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1253.01.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "spring onion, fresh, raw"
dictionary.df[n1,13] <- "allium cepa"

#Peas, fresh
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1242")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1242.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "peas, green, fresh, raw"
dictionary.df[n1,13] <- "pisum sativuma"

#Cowpea, fresh
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1241.9")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1241.9.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cow peas, green, fresh, raw"
dictionary.df[n1,13] <- "vigna unguiculata"

#Watermelon
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1221")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1221.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "watermelon, fresh, raw"
dictionary.df[n1,13] <- "citrullus lanatus"


## ├├  Other tropical fruits, n.e.c.. (1319) -----

food_desc <-  c("jackfruit, fresh, raw", 
                "passion fruit, dark purple skin, fresh, raw", 
                "rose apple, fruit, fresh, raw",
                "Java plum, fruit, fresh, raw", 
                "soursop, fruit, fresh, raw", 
                "carambola, fruit, fresh, raw", 
                "breadfruit, fresh, raw",
                "custard apple, fruit, fresh, raw"
                )

other_name <- c(rep(NA, 2), "malabar plum", 
                "jambolan, black plum, zambarau (KE18), malabar plum (used for s. cumini and s.jambos)", 
               NA, "star fruit (WA19, SL18)", NA,  "sweet sop (SL18), sweet apple (WA19)")

scientific_name <- c("artocarpus heterophyllus", "passiflora edulis",
                     "syzygium jambos", "syzygium cumini", "annona spp.",
                     "averrhoa carambola", "artocarpus altilis",
                     "annona squamosa")

taxo <- c(rep(NA, 4), "5468B", "545XT", rep(NA, 4))

# One input
fex2_new <- c(NA)
id2 <- "1319"
desc1 <-  c("Other tropical and subtropical fruits, n.e.c. This subclass includes: - durian fruits - soursop, sweetsop and other species of Annona - bilimbi, Averrhoa bilimbi - starfruit, carambola Averrhoa carambola - breadfruit, Artocarpus communis , Artocarpus incisa , Artocarpus integrifolia - pawpaw, Asimina - feijoas - fruit of various species of Sapindaceae, including: litchis, lychees, Litchi chinensis longan, mata kucing pulasan, Nephelium mutabile rambutan fruit, Nephelium Lappaceum - giant granadillas, granadillas, purple granadillas - jackfruit - langsat, langsat fruit, Lansium domesticum - passion fruit, Passiflora edulis, Passiflora quadrangularis - akee, Blighia sapida - pepinos, Solanum muricatum - rose hips - sapodilla, sapota (chico), Manilkara zapota, Achras zapota")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c("https://www.catalogueoflife.org/")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


#Pear
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1342.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1342.01.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pear, fresh, raw"
dictionary.df[n1,13] <- "pyrus communis"

#pickles, sweet

id2 <- "21340"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pickles, sweet"
dictionary.df[n1,13] <- NA

print(paste0(id2, ".01"))


#├ New item (ID_3) ----

#Juices, canned bottled, sweetened

id3 <- "21439.9.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "juices, canned bottled, unsweetened"





#Capsicum, red

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1231.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1231.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "capsicum, yellow, raw"
dictionary.df[n1,13] <- "capsicum annuum"

#Capsicum, red

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1231.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1231.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "capsicum, red, raw"
dictionary.df[n1,13] <- "capsicum annuum"


#Green beans

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1241.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1241.9.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "green beans, fresh, raw"
dictionary.df[n1,13] <- "phaseolus vulgaris"


#Broccoli

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1213.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1213.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "broccoli, fresh, raw"
dictionary.df[n1,13] <- "brassica oleracea var. italica"



#Courgette

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1235.03")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1235.04"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "courgette, green, fresh, raw"
dictionary.df[n1,10] <- "also called zucchini, baby marrow, summer squash"
dictionary.df[n1,13] <- "cucurbita pepo var. Cylindrica"


#Add - 1251 - carrot, fresh, raw
subset(dictionary.df, ID_2 == "1251")

#Manual inputs:
id2 <- "1251"
desc_new <- "carrot, fresh, raw"
fex2_new <- NA
scien_new <- "daucus carota"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new


#Add - 1251 - turnip, fresh, raw
subset(dictionary.df, ID_2 == "1251")

#Manual inputs:
id2 <- "1251"
desc_new <- "turnip, fresh, raw"
fex2_new <- NA
scien_new <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 1341 Apple, red, raw

#Manual inputs:
id2 <- "1341"
desc_new <- "apple, red, raw"
fex2_new <- NA
scien_new <- "malus domesticus"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 1341 Apple, green, raw

#Manual inputs:
id2 <- "1341"
desc_new <- "apple, green, raw"
fex2_new <- NA
scien_new <- "malus domesticus"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 1314 dates, fresh,raw

#Manual inputs:
id2 <- "1314"
desc_new <- "dates, fresh, raw"
fex2_new <- NA
scien_new <- "phoenix dactylifera"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 21393.9 baobab, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "baobab, leaves, dried, raw"
fex2_new <- NA
scien_new <- "adansonia digitata"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 21393.9  Bay, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "bay, leaves, dried, raw"
fex2_new <- NA
scien_new <- "laurus nobilis"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 21393.9  Cowpea, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "cowpea, leaves, dried, raw"
fex2_new <- NA
scien_new <- "vigna unguiculata"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 21393.9  False sesame, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "false sesame, leaves, dried, raw"
fex2_new <- NA
scien_new <- "ceratotheca sesamoides"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

# Add - 21393.9  Jute mallow, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "jute mallow, leaves, dried, raw"
fex2_new <- NA
scien_new <- "corchorus spp."
other_name <- "bush okra, leaves, dried raw (WA19)"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# Add - 21393.9  Okra, fruit, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "okra, fruit, dried, raw"
fex2_new <- NA
scien_new <- "abelmoschus esculentus"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# Add - 21393.9 Onion, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "onion, dried, raw"
fex2_new <- NA
scien_new <- "allium cepa"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# Add - 21393.9 Pumpkin, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "pumpkin, leaves, dried, raw"
fex2_new <- NA
scien_new <- "cucurbita pepo"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 21393.9 Sicklepod, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "sicklepod, leaves, dried, raw"
fex2_new <- NA
scien_new <- "senna tora"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 21393.9 Tamarind, leaves, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "tamarind, leaves, dried, raw"
fex2_new <- NA
scien_new <- "tamarindus indica"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


#Add - 21393.9 Thyme, dried, raw
#Manual inputs:
id2 <- "21393.9"
desc_new <- "thyme, dried, raw"
fex2_new <- NA
scien_new <- "brassica nigra"
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 21393.01 Mushroom, dried, raw
#Manual inputs:
id2 <- "21393.01"
desc_new <- "mushroom, dried, raw"
fex2_new <- NA
scien_new <- "agaricus spp."
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 21439.02 - lemon, juice, home squeezed
#Manual inputs:
id2 <- "21439.02"
desc_new <- "lemon, juice, homemade"
fex2_new <- NA
scien_new <- "citrus limon"
other_name <- NA
ref1 <- NA
desc2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- other_name
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- desc2
dictionary.df[n1,13] <- scien_new

#Add - 21439.04 - lime, juice
#Manual inputs:
id2 <- "21439.04"
desc_new <- "lime, juice"
fex2_new <- NA
scien_new <- "citrus × aurantiifolia"
other_name <- NA
ref1 <- NA
desc2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))


dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- other_name
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- desc2
dictionary.df[n1,13] <- scien_new

#Add - 21439.9 -  Passion Fruit, juice 
#Manual inputs:
id2 <- "21439.9"
desc_new <- "passion fruit, juice"
fex2_new <- NA
scien_new <- "passiflora edulisa"
other_name <- NA
ref1 <- NA
desc2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- other_name
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- desc2
dictionary.df[n1,13] <- scien_new

#Add - 21439.01 -  tangerine, juice, homemade
#Manual inputs:
id2 <- "21439.01"
desc_new <- "tangerine, juice, homemade"
fex2_new <- NA
scien_new <- "citrus reticulata"
other_name <- NA
ref1 <- NA
desc2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- other_name
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- desc2
dictionary.df[n1,13] <- scien_new

#Add - 1349.2 -  loquat, fresh, raw
#Manual inputs:
id2 <- "1349.2"
desc_new <- "loquat, fresh, raw"
fex2_new <- NA
scien_new <- "eriobotrya japonica"
taxo <- "4S4HG"
ref1 <- "https://www.catalogueoflife.org/data/taxon/4S4HG"
other_name <- "nispero"



#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


#Add - 21419.99 -  Baobab fruit, powderly part, dry, raw
#Manual inputs:
id2 <- "21419.99"
desc_new <- "baobab fruit, powderly part, dried, raw"
fex2_new <- NA
scien_new <- "adansonia digitata"
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1329 -  Kumquats, fresh, raw
#Manual inputs:
id2 <- "1329"
desc_new <- "kumquats, fresh, raw"
fex2_new <- NA
scien_new <- "citrus japonica or fortunella spp."
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1234 -  Tomato, red, ripe, raw
#Manual inputs:
id2 <- "1234"
desc_new <- "tomatoes, red, ripe, fresh, raw"
fex2_new <- NA
scien_new <- "solanum lycopersicum"
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1234 -  Tomato, green, raw
#Manual inputs:
id2 <- "1234"
desc_new <- "tomatoes, green, fresh, raw"
fex2_new <- NA
scien_new <- "solanum lycopersicum"
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1316 -  Mango, ripe, fresh, raw
#Manual inputs:
id2 <- "1316"
desc_new <- "mangoes, ripe, fresh, raw"
fex2_new <- NA
scien_new <- "mangifera indica"
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1316 -  Mango, unripe, fresh, raw
#Manual inputs:
id2 <- "1316"
desc_new <- "mangoes, unripe, fresh, raw"
fex2_new <- NA
scien_new <- "mangifera indica"
taxo <- NA
ref1 <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├  other vegetables, fresh n.e.c. (1290.9) -----

food_desc <-  tolower(c(
  "malabar spinach, leaves, fresh, raw",
  "spider plant, leaves, fresh, raw",
  "black nightshade, leaves, fresh, raw", 
  "native eggplant, fresh, raw",
  "taro, leaves, fresh, raw",
  "cow pea, leaves, fresh, raw",
  "sweet potato, leaves, fresh, raw",
  "celery, fresh, raw", "radish, fresh, raw",
  "hibiscus, leaves, fresh, raw", 
  "radish, round, red skin, fresh, raw",
  "radish, long, red skin, fresh, raw", 
  "swiss chard, leaves, fresh, raw", 
  "stinging nettle, leaves, fresh, raw", 
  "jute mallow, leaves, fresh, raw", 
  "blackjack, leaves, fresh, raw",
  "baobab, leaves, fresh, raw", 
  "moringa, leaves, fresh, raw",
  "Cocoyam, leaves, fresh, raw", 
  "Eggplant, leaves, fresh, raw",
  "False sesame, leaves, fresh, raw", 
  "Hibiscus cannabinus, leaves, fresh, raw",
  "Sicklepod, leaves, fresh, raw",
  "Tamarind, leaves, fresh, raw",
  "Vernonia, leaves, bitter, fresh, raw",
  "Vernonia, leaves, sweet, fresh, raw", 
  "Waterleaf, leaves, fresh, raw",
  "Wild spinach, leaves, fresh, raw"
  ))

other_name <- c("Vine (African) spinach (KE18)",
                "cat\\'s whiskers (Luni) (MW19)",
  rep(NA, 12), 
  "bush okra, leaves, dried raw (WA19), Leaves, jews mallow, raw, Corchorus trilocularis, (Denje)",
  rep(NA, 3))

other_info <- c("https://www.fondazioneslowfood.com/en/ark-of-taste-slow-food/nderema/#:~:text=Nderema%2C%20also%20known%20as%20vine,green%20or%20brownish%2Dpurple%20stems.", 
                rep(NA, 14),
                "https://www.healthbenefitstimes.com/blackjack/, (RESEWO, 2013)",
                rep(NA, 2))

scientific_name <- tolower(c("basella alba", "gynandropsis gynandra/cleome gynandra",
                     "solanum scabrum", "solanum macrocarpon",
                     "colocasia esculenta", "vigna unguiculata",
                    NA,  "apium graveolens", NA, 
                    "hibiscus sabdariffa", 
                     "raphanus sativus", 
                     "raphanus sativus", 
                    "beta vulgaris ssp. vulgaris", 
                    "urtica dioica", 
                    "corchorus spp.", 
                    "bidens pilosa",
                     "adansonia digitata",
                    "moringa stenopetala",
                    "Xanthosoma sagittifolium", 
                    "Solanum melongena",
                    "Ceratotheca sesamoides",
                    "Hibiscus cannabinus",
                    "Senna tora/Senna obtusifolia",
                    "Tamarindus indica",
                    "Gymnanthemum amygdalinum", 
                    "Baccharoides calvoana subsp. calvoana", 
                    "Talinum fruticosum",
                    "Gnetum africanum"))

fex2_new <- c(rep(NA, 14), "A00NY#F28.A07HS", rep(NA, 2))

# One input
id2 <- "1290.9"
desc1 <-  c("This subclass includes the fresh vegetables not elsewhere classified. They may not be identified separately because of their minor relevance at the international level. Because of their limited local importance, some countries report vegetables under this heading that are classified individually by FAO (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- other_info[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├  raspberries (1353.01)

food_desc <-  c("rasberry, fresh, raw")
other_name <- c(NA)
scientific_name <- c("Rubus idaeus")
taxon <- c("7L2DY")

# One input
fex2_new <- c(NA)
id2 <- "1353.01"
desc1 <-  c("This subclass covers raspberries, species of Rubus idaeus.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c("https://www.catalogueoflife.org/")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├  Cashewapple (1359.02) -----

food_desc <-  c("cashew apple, fruit, fresh, raw"
                )

other_name <- c(NA)

scientific_name <- tolower(c("Anacardium occidentale"))

taxon <- c( rep(NA, 1))

# One input
fex2_new <- c(NA)
id2 <- "1359.02"
desc1 <-  c("Cashewapple, species of Anacardium occidentale, is the thickened, fleshy stem below the cashew nut. When soft it is used for jam. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c("https://www.catalogueoflife.org/")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}



## ├├  Other fruits n.e.c. (1359.9) -----

food_desc <-  c("saba senegalensis, fresh, raw", 
                "doumpalm fruit, fresh, raw", 
                "mulberry, fresh, raw",
                "pomegranate, fresh, raw", 	
                "tree tomato, dark red skin, fresh, raw", 
                "star apple, fruit, African, fresh, raw",
                "yellow mombin, fresh, raw")

other_name <- c("gumvine (WA19), malombo (Sierra Leone)", rep(NA, 4), 
                "bobi wata (SL)", "hog-plum (WA19)")

scientific_name <- tolower(c("saba senegalensis", "Hyphaene thebaica", "Morus spp.", 
                     "Punica granatum", "Solanum betaceum", 
                     "Chrysophyllum albidum", 
                     "Spondias mombin"))

taxon <- c("6WYLF", "3NRL4", "5VCQ", "4QHX8", "4XZPN", rep(NA, 2))

# One input
fex2_new <- c(NA)
id2 <- "1359.9"
desc1 <-  c("Other fresh fruit that are not identified separately because of their minor relevance at the international level. Because of their limited local importance, some countries may report fresh fruit under this heading that are classified separately. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c("https://www.catalogueoflife.org/")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├  Juice of fruits n.e.c. (21439.9) -----

food_desc <-  c("lollies, made with fruit juice", 
                "cranberry apple, juice, bottled", 
                "cranberry apricot juice, bottled", 
                "cranberry grape juice, bottled", 
                "grape juice, canned", 
                 "orange apricot juice, canned", 
                "pineapple grapefruit juice, canned", 
                "pineapple orange juice, canned",
                "mango, nectar, canned"
                )

other_name <- c(NA)
scientific_name <- c(NA)
taxon <- c(NA)

# One input
fex2_new <- c(NA)
id2 <- "21439.9"
desc1 <-  c("Juice is obtained by mechanical extractors, or by pressing, and is then submitted to various processes. Unfermented, it may or may not be frozen. For direct consumption (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

#Add - 1313 - Plantain, deep fried in unfortified vegetable oil*
#Manual inputs:
id2 <- "1313"
desc_new <- "plantain, deep fried, unfortified"
fex2_new <- NA
scien_new <- NA
taxo <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

#Add - 1239.01 - Okra, leaves, fresh, raw
#Manual inputs:
id2 <- "1239.01"
desc_new <- "okra, leaves, fresh, raw"
fex2_new <- NA
scien_new <- "abelmoschus esculentus"
taxo <- NA
other_name <- NA


#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new




#Add - 21399.01 - Tomato, paste
#Manual inputs:
id2 <- "21399.01"
desc_new <- "tomato, paste"
fex2_new <- NA
scien_new <- NA
taxo <- NA
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

## ├├ Pineapples, otherwise prepared or preserved  (21491) -----

#Manual inputs:
food_desc <- c("pineapple, canned in syrup, whole contents")

other_name <- c(NA)

# Fixed
id2 <- "21491"
fex2 <- NA
scientific_name <- "ananas comosus"
desc1 <- "Fruit, nuts and peel, including frozen, prepared or preserved, jam, paste, marmalade, pure and cooked fruits, other than those listed separately (Unofficial definition)"
ref1 <- "https://www.fao.org/faostat/en/#data/SCL"


for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}

#Add - 21321 - Tomato, juice
#Manual inputs:
id2 <- "21321"
desc_new <- "tomato, juice"
fex2_new <- NA
scien_new <- NA
taxo <- NA
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

#Add - 21329 - Carrot, juice
#Manual inputs:
id2 <- "21329"
desc_new <- "carrot, juice"
fex2_new <- NA
scien_new <- NA
taxo <- NA
other_name <- NA
ref2 <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
dictionary.df[n1,14] <- ref2

## ├├ Fruit prepared (F0623) -----

#Manual inputs:
food_desc <- c("chutney, apple",
               "chutney, mango", 
               "banana, chips", 
              "fruit cocktail, canned in syrup, whole contents")

scientific_name <- c(rep(NA, 2), "musa spp.",
                     rep(NA, 1))

fex2 <- c(rep(NA, 4))

other_name <- c(rep(NA, 4))

# One time input
id2 <- "F0623"
desc1 <- "Fruit, nuts and peel, including frozen, prepared or preserved, jam, paste, marmalade, pure and cooked fruits, other than those listed separately (Unofficial definition)"
ref1 <- "https://www.fao.org/faostat/en/#data/SCL"


# Function:
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├ chillies and peppers, dry (capsicum spp. and pimenta spp.), raw (1652) -----

food_desc <-  c("chilli pepper, green, fresh, raw")
other_name <- c(NA)
scientific_name <- c("Capsicum frutescens")
taxon <- c(NA)

# One input
fex2_new <- c(NA)
id2 <- "1652"
desc1 <-  c("Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw This subclass includes: - raw, dried fruits of plants of the genus Capsicum capsicum, sweet pepper chilli pepper cultivars of Capsicum annuum cayenne pepper jalapeno pepper anaheim pepperpaprika, bell pepper ancho pepperpimento - raw, dried fruits of plants of the genus Pimenta allspice, Jamaica pepper Pimenta dioica This subclass does not include: - chillies and peppers, green, cf. 01231 - dry chillies and peppers, crushed, ground or otherwise processed, cf. 23922")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├ onions and shallots, dry (excluding dehydrated) (1253.02) -----

food_desc <-  c("onion, mature, red skinned, fresh, raw")
other_name <- c(NA)
scientific_name <- c("allium cepa")
taxon <- c(NA)

# One input
fex2_new <- c(NA)
id2 <- "1253.02"
desc1 <-  c("Onions and shallots, dry (excluding dehydrated), species of Allium cepa, includes onions at a mature stage, but not dehydrated onions. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  cabbages (1212) -----

food_desc <-  c("kale, leaves, fresh, raw", 
                "cabbage, red, fresh, raw",
                "kale, acephala,leaves, fresh, raw")

other_name <- c(rep(NA, 2), "Kenyan kale (sukuma wiki in swahili) (KE18)")

scientific_name <- c("brassica oleracea var. capitata", 
                     NA, 
                     "brassica oleracea var. acephala")
taxon <- c(NA)

# One input
fex2_new <- c(NA)
id2 <- "1212"
desc1 <-  c("Cabbages This subclass is defined through the following headings/subheadings of the HS 2007: 0704.20, .90.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├  sweet corn, prepared or preserved (21399.03) -----

food_desc <-  c("corn, sweet, in brine, canned")
other_name <- c(NA)
scientific_name <- c(NA)
taxon <- c(NA)

# One input
fex2_new <- c(NA)
id2 <- "21399.03"
desc1 <-  c("Prepared or preserved other than by vinegar or acetic acid; not frozen (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├ figs, dried (21419.02) -----

food_desc <-  c("figs, dried, raw")

scientific_name <- c("ficus carica")

other_name <- c(NA)

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "21419.02"
desc1 <-  c("Prepared or preserved other than by vinegar or acetic acid; not frozen (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├ apricots, dried  (21419.01) -----

food_desc <-  c("apricot, dried, raw")

scientific_name <- c(NA)

other_name <- c(NA)

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "21419.01"
desc1 <-  c("Prepared either by direct drying in the sun or by industrial processes. May be marketed as slices or blocks of pulp, either dried or evaporated (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├ plums, dried (21412) -----

food_desc <-  c("plums, dried, raw")

scientific_name <- c("prunus domestica")

other_name <- c("prune, dried, raw")

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "21412"
desc1 <-  c("Plums, dried This subclass is defined through the following headings/subheadings of the HS 2007: 0813.20.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├ fruit, nuts, peel, sugar preserved (23670.02) -----

food_desc <-  c("fruit cocktail, canned in syrup, drained", 
                 "pineapple, candied")

scientific_name <- c(rep(NA, 2))

other_name <- c(rep(NA, 2))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "23670.02"
desc1 <-  c("Fruit, nuts, fruit peel, and other parts of plants preserved with sugar (either drained, glazed or crystallized) (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


## ├├ cantaloupes and other melons (1229) -----

food_desc <-  c("melon, cantaloupe, fresh, raw", 
                "melon, honeydew, fresh, raw")

scientific_name <- tolower(c(rep("Cucumis melo", 2)))

other_name <- c(rep(NA, 2))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "1229"
desc1 <-  c("Cantaloupes and other melons This subclass is defined through the following headings/subheadings of the HS 2007: 0807.19.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

# ├├ bananas (1312) -----

food_desc <-  c("banana, sweet, unripe, fresh, raw", 
                "banana, yellow flesh, fresh, raw")

scientific_name <- tolower(c(rep("Musa spp.", 2)))

other_name <- c("banana, sweet, green (WA19)", rep(NA, 1))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "1312"
desc1 <-  c("Bananas This subclass includes: - sweet/dessert bananas, Musa sapientum, M. cavendishii, M. nana, i.e. bananas that can be eaten without further preparation This subclass does not include: - plantains, cooking bananas, Musa paradisiaca, cf. 01313")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}



# ├├ Locust beans (carobs) (1356) -----

food_desc <-  c("African locust bean, dried, raw", 
                "African locust bean, fermented (soumbala), dried")

scientific_name <- tolower(c(rep("Parkia biglobosa", 2)))

other_name <- c( rep(NA, 1), "sounbareh (SL18)")

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "1356"
desc1 <-  c("Locust beans (carobs) This subclass includes: - Locust beans (carobs), Ceratonia siliqua")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

########## Other foods (OT) ##############
subset(dictionary.df, ID_1 == "2659")

#Correcting category codes & description (ID_0 & FoodName_0)
#"2581"|ricebran oil and products                  
#"2582"|maize germ oil and products                
#"2657"|beverages, fermented and products          

dictionary.df$ID_0[dictionary.df$ID_1 %in% c("2659","2581", "2582",
                                             "2657") ] <- "OT"
dictionary.df$FoodName_0[dictionary.df$ID_1 %in% c("2659","2581", "2582","2657")] <- "Other foods"
#Adding scientific name  
dictionary.df$scientific_name[dictionary.df$ID_3 == "1449.9.01"] <- "guizotia abyssinica"

#Correcting food description by codes
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1460.01"] <- "coconut, mature, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23670.01.01"] <- "sweets, including chewing gum"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23511.02.01"] <- "sugar, from unrefined sugar cane"
dictionary.df$Description2[dictionary.df$ID_3 == "23511.02.01"] <- "jaggery, panela, raw sugar"
dictionary.df$Desc1.ref2[dictionary.df$ID_3 == "23511.02.01"] <- "https://doi.org/10.1016/j.foodchem.2017.01.134"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "24230.03.03"] <- "millet-fermented beverage"

#Correcting food item classification: 1- food item code, 2- sub-classification
subset(dictionary.df, ID_3 == "1699.05")
##Removing:: Amend yeast (1699.04) --> F1232.06
##Amend baking powder (1699.05) --> F1232.07
dictionary.df <- subset(dictionary.df, !ID_3 %in% c("1699.04", "1699.05"))
##Amend tabasco sauce (1699.06)  --> F1232.08 (Not updated)


#├ New category from ID_2 ----

#Molasses

id2 <- "23540"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "molasses"
dictionary.df[n1,13] <- NA

print(paste0(id2, ".01"))

#Brown sugar

id2 <- "2351F"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sugar, brown"
dictionary.df[n1,13] <- NA

print(paste0(id2, ".01"))

#Ginger

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1657")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1657.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "ginger, fresh, raw"
dictionary.df[n1,13] <- "zingiber officinale"

#├ New item (ID_3) ----

## ├├  anise, badian, coriander, cumin, (1654) -----
#caraway, fennel and juniper berries, raw  

food_desc <-  c("coriander, leaves, fresh, raw",
                "coriander, leaves, dried, raw",
                "coriander, seed, dried, raw")

scientific_name <- c(rep("coriandrum sativum", 3))

other_name <- c(NA)

fex2_new <- c(NA)
id2 <- "1654"
desc1 <-  "Anise, badian, coriander, cumin, caraway, fennel and juniper berries, raw This subclass includes: - aniseed, Pimpinella anisum, raw - star anise (badian) or Chinese star anise, Illicium verum, raw - fennel, Foeniculum vulgare, raw (when used as spice) - coriander (cilantro), Coriandrum sativum, raw - cumin, Cuminum cyminum, raw - caraway seeds, Carum carvi, raw - juniper berries, Juniperus communis, raw This subclass does not include: - fennel (when used as a vegetable), cf. 01290 - processed anise, fennel, coriander, cumin, caraway and juniper berries, cf. 23924"
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├  Food preparations (incl. sauces) (F1232) -----

food_desc <-  c("mayonnaise", "soup", "potash", "chilli sauce", "maize porridge", 
                "yeast, baking", "baking powder", 
                "baking soda, powder", "groundnut sauce", 
                "soup, tomato, condensed, canned", 
                "stock cube, beef", "stock cube, chicken", 
                "stock cube, vegetables", "stock cube, low sodium", 
                "cowpea, fried cakes", "maize dough, fermened, from maize grain", 
                "maize porridge, fermented from white maize, cooked", 
                "soup, chicken, beer yeast, vegetables and fermented African locus beans", 
                "soup, cabbage and vegetables", 
                "cassava and unripe plantain, mashed, cooked",                  
                "cassava and ripe plantain, mashed, cooked", 
                "yam and cassava, mashed, cooked", 
                "baked beans", "luncheon beef",
                "couscous, wheat", "falafel", 
                "pasta, macaroni, refined, dried, boiled",
                "pasta, spaghetti, refined, dried, boiled", 
                "beans, shellie, canned", 
                "rice, egg, fried", 
                "burger, hamburger, takeaway",
                 "kebab in pitta bread with salad",
                "injera, teff grain, cooked",
                "plantain chips, salted", 
                "meat pastry", "sausage roll, homemade")

scientific_name <- c(rep(NA,16), "zea mays", rep(NA, 7), "triticum durum",
                     rep(NA,11))

other_name <- c(rep(NA,7), "bicarbonate of soda", NA, NA, 
                "beef seasoning cube", "chicken seasoning cube", 
                "vegetable seasoning cube", "low Na seasoning cube" ,
                "boussan touba (Burkina Faso) (WA19), beans akara (Sierra Leone)", 
                "kenkey (WA19), agidi (stiff maize dough) (Sierra Leone)", 
                "ogi (WA19), pap (Sierra Leone)", 
                rep("recipe from Burkina Faso in WA19", 2), 
                rep("Banakou né (Burkina Faso) (WA19), Yebbe (Sierra Leone)", 3), 
                rep(NA, 5), "egg, fried rice", 
                rep(NA, 6), "sausage roll (UK21), meat pie (JA15)")

fex2_new <- c(rep(NA, 5), "A049A#F02.A06CK$F01.A066J$F27.A049A", 
              "A048Q#F02.A06CG", 
              rep(NA, 30))

# Fixed input
id2 <- "F1232"
desc1 <-  "Including both crop and livestock products. Inter alia: homogenized composite food preparations; soups and broths; ketchup and other sauces; mixed condiments and seasonings; vinegar and substitutes; yeast and baking powders; stuffed pasta, whether or not cooked; couscous; and protein concentrates. Include inter alia: turtle eggs and birds' nests. (Unofficial definition)"
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├  sugar confectionery (23670.01) -----

food_desc <- c("candies, hard", "chewing gum", 
               "jelly, fruits")

fex2_new <- c(rep(NA, 2), 
              "A16FC#F02.A16FD$F28.A0BA1$F04.A0BY6$F04.A04RK")

other_name <- c(rep(NA, 2), "fruit gums (UK21), jellies (US19)")

#Manual inputs:
id2 <- "23670.01"
scientific_name <- c(NA)

desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


#Tea with milk

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23914.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23914.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "tea with milk"


#Coconut, inmature

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1460.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1460.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "coconut, inmature, fresh, raw"
dictionary.df[n1,13] <- "cocos nucifera"


## ├├ Infant food (23991.01) -----

food_desc <- c("infant formula, 3 months, fortified, powder", 
               "infant formula, 6 months, fortified, powder", 
               "infant food, whole wheat, with apples, fortified, dried",
               "infant food, wheat, biscuits",
               "infant food, cereal, mixed, with bananas, dried",
               "infant food, cereal, oatmeal, with bananas, dried",
               "infant food, cereal, rice, with bananas, dried",
               "infant food, cereal, rice, brown, dried",
               "infant food, cereal, oatmeal, with honey, dried"
               )

other_name <- c(rep(NA, 2), 
                "baby food, whole wheat, with apples, fortified, dried",
                "baby food, GERBER, GRADUATES Lil Biscuits Vanilla Wheat (US19)",
                rep(NA, 5))

fex2_new <- c(rep(NA, 9)) 

#Manual inputs:
id2 <- "23991.01"
scientific_name <- NA
desc1 <-  c("Preparations for infant consumption, usually containing some non-cereal ingredient (Unofficial definition)")
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}


#Add - 21700.02 - shortening
#Manual inputs:
id2 <- "21700.02"
desc_new <- "shortening"
fex2_new <- NA
scien_new <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new


#Add - 1802 - sugar cane, juice, raw
#Manual inputs:
id2 <- "1802"
desc_new <- "sugar cane, juice, raw"
fex2_new <- NA
scien_new <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new




# 23914 -  Tea, green, infusion
#Manual inputs:
id2 <- "23914"
desc_new <- "tea, green, infusion"
fex2_new <- NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new

# 23914 - Tea, herbal, infusion
#Manual inputs:
id2 <- "23914"
desc_new <- "tea, herbal, infusion"
fex2_new <- NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├ other non-alcoholic caloric beverages n.e.c (24490) -----

food_desc <- tolower(c("beverage, energy, carbonated", 
               "beverage, RED BULL, energy, carbonated", 
               "Water, non-carbonated, natural fruit flavors, sweetened"))

other_name <- c(rep("energy drink",2),
                "flavoured, sweetened water")

fex2_new <- c(NA)

#Manual inputs:
id2 <- "24490"
desc1 <-  c("Other non-alcoholic caloric beverages This subclass is defined through the following headings/subheadings of the HS 2007: 2202.")
ref1 <-  c(NA)
scientific_name <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}



## ├├  Beer of barley, malted (24310.01) -----

food_desc <-  c("beer, special (>6% v/v alcohol)",
                  "beer, stout", 
                "malt beverage, non-alcohol"
)

other_name <- c(rep(NA, 1), 
                "Guiness", "Maltina")

fex2_new <- c(NA)

#Manual inputs:
# One input
id2 <- "24310.01"
desc1 <-  c("Beverage that may be alcoholic or non-alcoholic that is made from fermented malted cereals (mainly barley), water and hops. Non-malted cereals may also be used. The FAO definition differs from the main international classifications in that it includes non-alcoholic beer (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
scientific_name <- c(NA)
taxon <- c(NA)
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}



# 1651 - Pepper, white
#Manual inputs:
id2 <- "1651"
desc_new <- "pepper, white"
fex2_new <- NA
scien_new <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new


## ├├  melonseed (1449.01) -----

desc_new <- c("melon seed, dried, raw", 
              "melon seed  (citrullus spp.), dried, raw")
              

scien_new <- c("cucumeropsis spp.", "Citrullus spp.")
ref1 <- NA
other_name <- NA


# Manual inputs:
id2 <- "1449.01"
fex2_new <- NA
ref1 <- NA
other_name <- NA

# Function: 
for(i in 1:length(desc_new)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- desc_new[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}



# 1449.9 - Benniseed, dried, raw 
#Manual inputs:
id2 <- "1449.9"
desc_new <- "benniseed, dried, raw"
fex2_new <- NA
scien_new <- "sesamum radiatum"
ref1 <- NA
other_name <- NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

#New entry - generation:
dictionary.df[n1,] <- dictionary.df[n2,]
#New entry - population:
dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new



## ├├  Chocolate products nes (F0666) -----

food_desc <- c( "chocolate almond milk, beverages",                  
                  "chocolate syrup, beverage", 
                "baking chocolate, unsweetened, squares", 
                "candies, chocolate covered, caramel with nuts", 
                "chocolate, drinking, powder") 

other_name <- c(rep(NA, 4), "milo")

for(i in 1:length(food_desc)){
 
#Manual inputs:
 id2 <- "F0666"
 desc_new <- 
 fex2_new <- NA
 scien_new <- NA
 ref1 <- NA

 
 #Auto inputs:
 id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
 id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                  str_replace(id3, "[[:alnum:]]{1,3}$",
                              formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                      width=2, flag=0)[2]))
 
 n1 <- dim(dictionary.df)[1]+1
 
 n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
              which(dictionary.df$ID_3 %in% id3))
 
 #New entry - generation:
 dictionary.df[n1,] <- dictionary.df[n2,]
 #New entry - population:
 dictionary.df[n1,7] <- id3_new
 dictionary.df[n1,8] <- fex2_new
 dictionary.df[n1,9] <- food_desc[i]
 dictionary.df[n1,11] <- ref1
 dictionary.df[n1,12] <- other_name[i]
 dictionary.df[n1,13] <- scien_new
}

## ├├  kola nuts (1379.02) -----

desc_new <- c("kola nuts, fresh, raw")


  # Manual inputs:
  id2 <- "1379.02"
  fex2_new <- NA
  scien_new <- "cola nitida"
  ref1 <- NA
  other_name <- NA

   #Auto inputs:
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name  

# 21422 - almond (shelled)
  # Manual inputs:
  id2 <- "21422"
  desc_new <- "almond, raw"
  fex2_new <- NA
  scien_new <- "prunus dulcis"
  ref1 <- NA
  other_name <- NA
  
  #Auto inputs:
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name  
  
  # 1375 - pistachio (in shel)
  # Manual inputs:
  id2 <- "1375"
  desc_new <- "pistachio, raw"
  fex2_new <- NA
  scien_new <- "pistacia vera"
  ref1 <- NA
  other_name <- NA
  
  #Auto inputs:
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- desc_new
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name
  
  
## ├├  Oil, other and products (including fat) (34550) -----
  
  #Manual inputs:
  food_desc <- c("vegetable oil",
                 "vegetable oil, fortified (GHA, NGA, SLE)", 
                 "vegetable oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- c(NA)
  
  other_name <- c(NA)

#Manual inputs:  
  id2 <- "34550"
  fex2_new <- c(NA)
  desc1 <-  c(NA)
  ref1 <-  c(NA)

  
# Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1[i]
    dictionary.df[n1,11] <- ref1[i]
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  
## ├├  sugar and syrups (23210.04) -----
  
  #Manual inputs:
  food_desc <- c("syrup, sugar cane")
  scientific_name <- c(NA)
  fex2_new <- c(NA)
    
  id2 <- "23210.04"
  desc1 <-  c("Includes invert sugar, caramel, golden syrup, artificial honey, maltose other than chemically pure, sorghum and palm sugars. See also the general note in the introduction (Unofficial definition)")
  ref1 <-  c(NA)
  other_name <- c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1[i]
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  
## ├├  other stimulant, spice and aromatic crops, n.e.c. (1699) -----
  
  #Manual inputs:
  food_desc <- c("curry powder", 
                 "ginger, dried, powder", 
                 "grains of selim, pepper, dried",
                 "parsley, leaves, fresh, raw", 
                 "parsley, leaves, dried, raw")
  
  other_name <- c(rep(NA, 2), "Guinea pepper, Uda (Ethiopian pepper) (WA19), parmanji (SL)",
                  rep(NA, 2))
  scientific_name <- c(rep(NA, 2), "xylopia aethiopica",
                       "petroselinum crispum", 
                       "petroselinum crispum")
  fex2_new <- c(rep(NA, 2))
  
  
  # Fixed
  id2 <- "1699"
  desc1 <-  c("Other stimulant, spice and aromatic crops, n.e.c. This subclass includes: - saffron, Crocus sativus - turmeric, Curcuma, Indian saffron, Curcuma longa - dill and dill seeds, Anethum graveolens - curry powders and pastes - thyme, Thymus - bay leaves, Laurus nobilis - Guinea pepper, negro pepper, seeds of Xylopia aethiopica - angelica stems This subclass does not include: - locust beans (carobs), cf. 01356 - unroasted chicory roots, varieties Cichorium intybus sativum, cf. 01691 - unroasted chicory roots, varieties other than Cichorium intybus sativum, cf. 01961 - sugar cane, cf. 01802 - sweet sorghum, Sorghum saccharatum, cf. 01809 - guarana nuts, cf. 01930 - kava, cf.01930 - basil and basil seeds, Ocinum basilicum, cf. 01930 - apricot, peach and plum stones and kernels, cf. 21499 - roasted chicory and other roasted coffee substitutes, cf. 23912")
  ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1[i]
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }

## ├├  spirits, liqueurs and other spirituous beverage (<80%) (2413) -----
  
  #Manual inputs:
  food_desc <- c("palm wine" )
  scientific_name <- c(NA)
  fex2_new <- c(NA)
  
  id2 <- "2413"
  desc1 <-  c("Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages")
  ref1 <-  c(NA)
  other_name <- c("poyo (SL)")
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1[i]
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├  other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c. (1379.9) -----
  
  #Manual inputs:
  food_desc <- tolower(c("bitter cola, seeds, dried, raw", 
                 "bitter cola, seeds, fresh, raw",
                 "Dikanut, kernel, dried, raw"))
  
  scientific_name <- tolower(c("Garcinia kola",
                               "Garcinia kola",
                               "Irvingia gabonensis"))
  fex2_new <- c(rep(NA,2))
 
   
  id2 <- "1379.9"
  desc1 <-  c("This subclass includes other nuts that are not identified separately because of their minor relevance at the international level. It includes, inter alia: Carya illinoensis (pecan nut) Caryocar nuciferum (butter or swarri nut) Canarium spp. (pili nut, Java almond, Chinese olives) Lecythis zabucajo (paradise or sapucaia nut) Macadamia ternifolia (Queensland, macadamia nut) Pinus pinea (pignolia nut) (Unofficial definition)")
  ref1 <-  c(NA)
  other_name <- c(rep(NA,2))
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1[i]
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├ Palm oil (2165) -----
  
  food_desc <- c("palm oil, refined, fortified (GHA, NGA, SLE)", 
                 "palm oil, refined, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)",
                 "palm oil, red")
  
  scientific_name <- tolower(c("Elaeis guineensis"))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "2165"
  desc1 <-  c("Palm oil")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├ Oil of palm kernel (21691.14) -----
  
  food_desc <- c("palm kernel oil, refined, fortified (GHA, NGA, SLE)", 
                 "palm kernel oil, refined, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Elaeis guineensis"))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "21691.14"
  desc1 <-  c("Obtained from the kernel of the nut of the fruits of the oil palm by pressure in two or three stages at different temperatures. Including oil of babassu kernels (Unofficial definition)")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├ butter of karite nuts (21691.03) -----
  
  food_desc <- c("shea butter")
  
  scientific_name <- tolower(c("Butyrospermum parkii/Vitellaria paradoxa"))
  
  other_name <- c("shea nut oil")
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "21691.03"
  desc1 <-  c("Obtained from the kernel of the nut of the fruits of the oil palm by pressure in two or three stages at different temperatures. Including oil of babassu kernels (Unofficial definition)")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├ Groundnut oil (2162) -----
  
  food_desc <- c("groundnut oil, fortified (GHA, NGA, SLE)", 
                 "groundnut oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Arachis hypogaea"))
  
  other_name <- c("peanut oil, fortified (GHA, NGA, SLE)", 
                  "peanut oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "2162"
  desc1 <-  c(NA)
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
## ├├ Coconut oil (2166) -----
  
  food_desc <- c("coconut oil, fortified (GHA, NGA, SLE)", 
                 "coconut oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Cocos nucifera"))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "2166"
  desc1 <-  c("Coconut oil")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  ## ├├ Soybean oil (2161) -----
  
  food_desc <- c("soybean oil, fortified (GHA, NGA, SLE)", 
                 "soybean oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Glycine max"))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "2161"
  desc1 <-  c("Soya bean oil")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  ## ├├ Sunflower-seed oil (21631.01) -----
  
  food_desc <- c("sunflower oil, fortified (GHA, NGA, SLE)", 
                 "sunflower oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Helianthus annuus"))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "21631.01"
  desc1 <-  c("Obtained by pressure extraction. Mainly for food use (Unofficial definition)")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  ## ├├ cocoa butter, fat and oil (23620) -----
  
  food_desc <- c("cocoa butter"
                )
  
  scientific_name <- c(NA)
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "23620"
  desc1 <-  c("Cocoa butter, fat and oil This subclass is defined through the following headings/subheadings of the HS 2007: 1804.")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
 
  ## ├├ cottonseed oil and products (2168) -----
  
  food_desc <- c("cotton seed oil, fortified (GHA, NGA, SLE)", 
                 "cotton seed oil, fortified (BEN, BFA, CIV, GIN, GNB, LBR, MLI, NER, SEN, TGO)")
  
  scientific_name <- tolower(c("Gossypium spp."))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "2168"
  desc1 <-  c("Cottonseed oil")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  ## ├├ mustardseed oil, crude (21641.02) -----
  
  food_desc <- c("mustard seed oil")
  
  scientific_name <- tolower(c("Brassica spp."))
  
  other_name <- c(NA)
  
  fex2_new <- c(NA)
  
  #Manual inputs:
  id2 <- "21641.02"
  desc1 <-  c("Obtained by dry pressure extraction. Has both food and industrial uses (Unofficial definition)")
  ref1 <-  c(NA)
  
  # Function: 
  for(i in 1:length(food_desc)){
    
    id2 <- id2
    
    id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
    id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                     str_replace(id3, "[[:alnum:]]{1,3}$",
                                 formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                         width=2, flag=0)[2]))
    
    n1 <- dim(dictionary.df)[1]+1
    
    n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
                 which(dictionary.df$ID_3 %in% id3))
    
    #New entry - generation:
    dictionary.df[n1,] <- dictionary.df[n2,]
    #New entry - population:
    dictionary.df[n1,7] <- id3_new
    dictionary.df[n1,8] <- fex2_new[i]
    dictionary.df[n1,9] <- food_desc[i]
    dictionary.df[n1,10] <- desc1
    dictionary.df[n1,11] <- ref1
    dictionary.df[n1,12] <- other_name[i]
    dictionary.df[n1,13] <- scientific_name[i]
  }
  
  
########## Roots and Tubers (RT) ##############
  
#Changing name cocoyam and adding scientific name (1591.01)
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1591.01"] <- "cocoyam, white, fresh, raw"
dictionary.df$Description1[dictionary.df$ID_3 == "1591.01"] <- "Also called tannias, and yautia"
dictionary.df$Description1[dictionary.df$ID_3 == "1591.01"] <- "xanthosoma sagittifolium"
#Fixing ID_3
dictionary.df$ID_3[dictionary.df$ID_3 == "23170.01"] <- "23170.01.01"


## ├├  Cassava, dried (1520.02) -----

food_desc <- c("cassava, root, white, dried, raw", 
               "cassava, root, sweet, dried, raw")

scientific_name <- c("manihot esculenta")

other_name <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
id2 <- "1520.02"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├  Edible roots and tubers  (1599.1) -----
####with high starch or inulin content, n.e.c., fresh 

food_desc <- c("ensete, fresh, raw", 
               "arrowroot, peeled, fresh, raw")

scientific_name <- c("ensete ventricosum", 
   "maranta arundinacea")

other_name <- c("false banana", NA)

other_info <- c("https://iopscience.iop.org/article/10.1088/1748-9326/ac40b2", NA)

fex2_new <- c(rep(NA, 2))

#Manual inputs:
id2 <- "1599.1"
desc1 <-  c("Edible roots and tubers with high starch or inulin content, n.e.c., fresh includes among all: Arracacoa xanthorrhiza (arracacha), Maranta arundinacea (arrowroot), Cyperus esculentus (chufa), Metroxylon spp. (sago palm), Oxalis tuberosa and Ullucus tuberosus (oca and ullucu), Pachyrxhizus erosus and Pachyrxhizus angulatus (yam bean, jicama), Tropaeolum tuberosum (mashua), Helianthus tuberosus (Jerusalem artichoke, topinambur). Other tubers, roots or rhizomes, fresh, which are not identified separately because of their minor relevance at the international level. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- other_info[i]
}



#├ New item (ID_3) ----


#Cocoyam, yellow

id3 <- "1591.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(str_extract(id3, "[[:alnum:]]{2,5}\\.\\d{1}"),
                              as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cocoyam, yellow, fresh, raw"
dictionary.df[n1,13] <- "xanthosoma sagittifolium"


## ├├  Sweet potatoes (1530) -----

food_desc <- c(
               "sweet potato, yellow flesh, fresh, raw",
               "sweet potato, deep yellow flesh, fresh, raw")

other_name <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
id2 <- "1530"
desc1 <-  c(NA)
ref1 <-  c(NA)
scientific_name <- tolower(c("Ipomoea batatas")) 


# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├  Cassava, flour (23170.01) -----

food_desc <- c( "cassava, flour, white, fermented, dried, raw", 
                "cassava, paste, white, fermented, raw",
                "cassava, grated, white, fermented, toasted, raw",
                "cassava, flour, white, cooked, fermented, dried, raw")

other_name <- c("dry fufu (WA19)", 
                "wet fufu (WA19)",
                "white gari (WA19)",
                "alibo/elubo/lafun (WA19)")

scientific_name <- tolower(c(rep("manihot esculenta", 3)))


#Manual inputs:
id2 <- "23170.01"
desc1 <-  c(NA)
ref1 <-  c(NA)
fex2_new <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


## ├├  cassava, fresh (1520.01) -----

food_desc <- c("cassava, root, fresh, fried (chips)", 
               "cassava, root, fresh, baked")

scientific_name <- c("manihot esculenta")

other_name <- c(NA)

fex2_new <- c(NA)

#Manual inputs:
id2 <- "1520.01"
desc1 <-  c(NA)
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

#Add - 1540 - Yam, Chinese

yam <- c("Yam, Chinese yam, \"Ichoimo\", tuberous root, raw",
"Yam, Chinese yam, \"Nagaimo\", tuberous root, raw",
"Yam, Chinese yam, \"Yamatoimo\", tuberous root, raw",
"Yam, tuber, combined cultivars, raw (Nigeria)")

for(i in 1:length(yam)){
#Manual inputs:
id2 <- "1540"
desc_new <- yam[i]
fex2_new <- NA
scien_new <- "Dioscorea spp."
taxo <- NA
ref1 <- NA
other_name <-NA

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 str_replace(id3, "[[:alnum:]]{1,3}$",
                             formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                     width=2, flag=0)[2]))
n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- taxo
dictionary.df[n1,11] <- ref1
dictionary.df[n1,12] <- other_name
dictionary.df[n1,13] <- scien_new
}


# Final Formatting ----

dictionary.df$FoodName_2 <- str_squish(dictionary.df$FoodName_2)

dictionary.df <- dictionary.df %>% mutate(across(c("FoodName_1", "FoodName_2", "FoodName_3",
                                  "scientific_name"), str_squish))

#dictionary.df[, c("FoodName_1", "FoodName_2", "FoodName_3",
#              "scientific_name")] <- apply(dictionary.df[, c("FoodName_1", "FoodName_2", "FoodName_3",
#                                      "scientific_name")], 2, tolower) 

dictionary.df <- dictionary.df %>% arrange(.)



# Save an R object - Running MAPS_dict_QC.R
saveRDS(dictionary.df, file = here::here("inter-output", "dictionary.df.rds"))

# Loading path - untracked file w/ personal WD
# source(here::here("path.R"))
# saveRDS(dictionary.df, file = paste0(path, "/data/dictionary.df.rds"))

