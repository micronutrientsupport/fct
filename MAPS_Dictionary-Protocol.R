
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

#Adding a new item for ihs5
#mucuna == 837 == 1701.4 == velvet bean, dried, raw 

#dictionary.df <- dictionary.df %>% add_row(
#  ID_0 = "PB",
#  FoodName_0 = "Pulses and Beans", 
#  ID_1 = 2546,
#  FoodName_1 = "beans and products", 
#  ID_2 = "1701", 
#  FoodName_2 =  "beans, dry",
#  ID_3 = "1701.04",
#  FE2_3 = NA,
#  FoodName_3 = "velvet bean, dried, raw") 

#dictionary %>% 
#write.csv(here::here('MAPS_Dictionary_v2.5.csv'), row.names = F)

#dictionary %>% select(ID_3, FoodName_3) %>% filter(str_detect(ID_3, "\\b")) %>% 
# rename(food_genus_id = "ID_3",
#       food_genus_name = "FoodName_3") %>% 
# write.csv(here::here('output', 'MAPS_Dictionary_v2.5.csv'), row.names = F)


dictionary.df <- read.csv(here::here("metadata", "MAPS_Dictionary_v2.6.csv")) %>% 
  select(-starts_with("X"))

dictionary.df$scientific_name <- NA

colnames(dictionary.df)

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
    FoodName_0 = "Fruits and Vegetable", 
    ID_1 = 2617,
    FoodName_1 = "apples and products", 
    ID_2 = "21435.01", 
    FoodName_2 = "apple juice",
    ID_3 = "21435.01.01",
    FE2_3 = "A039M#F10.A077J", #We used qualitative info w/ added sugar to specify that it's sweetened 
    FoodName_3 = " apple juice, sweetened")  #we did not used the #sweetened agent bc we did not know 
#the sweetened used. 


#making a distinction between two rice that were named the same
#we will use local rice whenever possible 
#default rice white == "23161.02.01"

dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.01.01"] <- "rice grain, imported, white, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.02.01"] <- "rice grain, local, white, raw"

#Found a issue with some ID_0 classification
#Need to change them to oil and seeds. 

#PB Pulses and Beans "2561",             sesame seed and products
#PB Pulses and Beans "2571",            soyabean oil and products
#PB Pulses and Beans "2572",           groundnut oil and products
#PB Pulses and Beans "2579",          sesameseed oil and products

#Found a issue with some ID_0 classification
#Need to change them to Other foods. 

#PB Pulses and Beans 2561             sesame seed and products
#PB Pulses and Beans 2571            soyabean oil and products
#PB Pulses and Beans 2572           groundnut oil and products
#PB Pulses and Beans 2579          sesameseed oil and products

#checking what items with duplicated ids
dictionary.df %>% filter(ID_1 %in% c("2561",
                                     "2571",
                                     "2572",
                                     "2579"))

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

dictionary.df <- dictionary.df %>% add_row(
  ID_0 = "CE",
  FoodName_0 = "Cereals", 
  ID_1 = 2520,
  FoodName_1 = "cereals, other and products", 
  ID_2 = "1199.9", 
  FoodName_2 =  "other cereals n.e.c.",
  ID_3 = "1199.9.01",
  FE2_3 = "",
  FoodName_3 = "teff grain, dried, unrefined, raw") %>% 
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
    ID_2 = "1702", 
    FoodName_2 =  "broad beans and horse beans, dry",
    ID_3 = "1702.01",
    FE2_3 = "",
    FoodName_3 = "horse bean, dried, raw") %>% 
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
    ID_2 = "1599.1", 
    FoodName_2 =  "edible roots and tubers with high starch or inulin content, n.e.c., fresh",
    ID_3 = "1599.1.01",
    FE2_3 = "",
    FoodName_3 = "ensete, raw", 
    Description1 = "it is often called false banana",
    Desc1.ref = "https://iopscience.iop.org/article/10.1088/1748-9326/ac40b2") %>% 
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
    Description1 = "bula") %>% 
  add_row(
    ID_0 = "CE",
    FoodName_0 = "Cereals", 
    ID_1 = 2520,
    FoodName_1 = "cereals, other and products", 
    ID_2 = "23140.08", 
    FoodName_2 =  "cereal preparations",
    ID_3 = "23140.08.01",
    FE2_3 = "",
    FoodName_3 = "injera, teff grain, ready-to-eat")

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
    FoodName_3 = "fenugreek, dried, raw") %>%
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
    ID_2 = "1212", 
    FoodName_2 =  "cabbages",
    ID_3 = "1212.04",
    FE2_3 = "",
    FoodName_3 = "kale, cabbage leaves, raw") %>%
  add_row(
    ID_0 = "FV",
    FoodName_0 = "Fruits and Vegetables", 
    ID_1 = 2605,
    FoodName_1 = "vegetables, other and products", 
    ID_2 = "1252", 
    FoodName_2 =  "green garlic", #http://datalab.review.fao.org/datalab/caliper/web/concept-page/0406-garlic
    ID_3 = "1252.01",
    FE2_3 = "",
    FoodName_3 = "garlic, fresh, raw") %>% 
  add_row(
    ID_0 = "OT",
    FoodName_0 = "Other foods", 
    ID_1 = 2645,
    FoodName_1 = "spices, other and products", 
    ID_2 = "1699", 
    FoodName_2 =  "other stimulant, spice and aromatic crops, n.e.c.",
    ID_3 = "1699.08",
    FE2_3 = "",
    FoodName_3 = "moringa, leaves, raw") %>%
  add_row(
    ID_0 = "FV",
    FoodName_0 = "Fruits and Vegetables", 
    ID_1 = 2605,
    FoodName_1 = "vegetables, other and products", 
    ID_2 = "1290.9", 
    FoodName_2 =  "other vegetables, fresh n.e.c.", 
    ID_3 = "1290.9.01",
    FE2_3 = "",
    FoodName_3 = "beetroot, raw")  %>% 
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

#Chapati, white, fortified

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F0020.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F0020.06"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "chapati, wheat flour, refined, fortified ghee"
dictionary.df[n1,10] <- NA
dictionary.df[n1,11] <- NA

#Chapati, with ghee

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F0020.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F0020.05"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "chapati, wheat flour, unrefined, added ghee"
dictionary.df[n1,10] <- "Also called \"Indian chapati\""
dictionary.df[n1,11] <- NA

#Chapati, brown

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F0020.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F0020.04"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "chapati, wheat flour, unrefined"
dictionary.df[n1,10] <- NA
dictionary.df[n1,11] <- NA


#Chapati, white

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F0020.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F0020.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "chapati, wheat flour, refined"
dictionary.df[n1,10] <- NA
dictionary.df[n1,11] <- NA


#Macaroni
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23710.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23710.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pasta, macaroni, wheat, dried, raw"
dictionary.df[n1,10] <- NA
dictionary.df[n1,11] <- NA

#Mandazi

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F0022.04")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F0022.08"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "dough, fried"
dictionary.df[n1,10] <- "mandazi"
dictionary.df[n1,11] <- "TZFCT, KENFCT"

#rice, brown, boiled

id3 <- "23162.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,9] <- "rice, brown, boiled"

#Add - 23161.01 - rice, parboiled, imported, raw
#Manual inputs:
id2 <- "23161.01"
desc_new <- "rice, parboiled, imported, raw"
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

#Add - 23161.01 - rice, parboiled, imported, boiled
#Manual inputs:
id2 <- "23161.01"
desc_new <- "rice, parboiled, imported, boiled"
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

#Add - 23161.02 - rice, parboiled, local, raw
#Manual inputs:
id2 <- "23161.02"
desc_new <- "rice, parboiled, local, raw"
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

#Add - 23161.02 - rice, parboiled, local, boiled
#Manual inputs:
id2 <- "23161.02"
desc_new <- "rice, parboiled, local, boiled"
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

#Add - F0020 - bread, sweet, wheat flour, refined
#Manual inputs:
id2 <- "F0020"
desc_new <- "bread, sweet, wheat flour, refined"
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


### Animal products (AP) ----
##NOTES:
##Any "whole fat would be under raw milk

#Fixing codes
#Chicken prep.
dictionary.df$ID_2[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"] <- "F1061"
dictionary.df$FoodName_2[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"] <- "poultry meat preparations"
dictionary.df$ID_3[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"]  <- "F1061.01"
#beef prep.
dictionary.df$ID_2[dictionary.df$FoodName_3 == "beef, fresh, grilled"] <- "F0875"
dictionary.df$ID_3[dictionary.df$FoodName_3 == "beef, fresh, grilled"]  <- "F0875.01"

#Fixing food desc
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21111.01.01"] <- "beef, lean, with bones, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "21111.02.01"] <- "beef, lean, without bones, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1501.02"] <- "North African catfish, fresh, raw"

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

#Beef sausages

id2 <- "21184.01"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "beef, sausage, raw"
dictionary.df[n1,13] <- "bos taurus"

#Pork sausages

id2 <- "21184.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "pork, sausage, raw"
dictionary.df[n1,13] <- "sus scrofa domesticus"

#Skimmed milk

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "22110.02")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "22110.02.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "milk, cow, skimmed, raw"
dictionary.df[n1,13] <- "bos taurus"


##├├  Fish -----

# Pelagic fish fillet

fish_name <- tolower(c("Barracudas nei", "Seerfishes nei", "Sardinellas nei",
                       "True tunas nei"))

isscaap <- c( "37", "36", "35", "36")

taxo <- c("17710001XX", "17501015XX", "12105012XX", "17501026XX")

alpha <- c("BAR", "KGX", "SIX", "TUS")

other_name <- c("Barracuda", "Mackerel", "Sardines, Dagaa", "Tuna" )

fish_scientific <- tolower(c("Sphyraena spp.", "Scomberomorus spp.", 
                             "Sardinella spp." , "Thunnus spp."))

for(i in 1:length(fish_name)){
  
  id2 <- "1529"
  
  n1 <- dim(dictionary.df)[1]+i
  
  n2 <- which(dictionary.df$ID_2 %in% id2)
  
  dictionary.df[n1,] <- dictionary.df[n2,]
  
  dictionary.df[n1,7] <- paste0(id2, ".0", i)
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(fish_name[i], ", fillet, fresh, raw")
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
  dictionary.df[n1,12] <- paste0("also called ", other_name[i])
  dictionary.df[n1,13] <- fish_scientific[i]
  
}


#Fish loop 
#we added several fishes from the same category
#Info of the English names, and other variables were taken 
#from the ASFIS dataset and from the Fisheries Global NCT
#work
#https://www.fao.org/fishery/en/collection/asfis/en

# Freshwater fish fillet

fish_name <- tolower(c("West African lungfish", "Nile perch" ,"Nile tilapia" ,        
                       "Rhinofishes nei","Naked catfishes","Upsidedown catfishes" ,
                       "North African catfish" ,"Tilapias nei"))

isscaap <- c( "13", "13" ,"12" ,"11", "13", "13" ,"13" ,"12")

taxo <- c("1160200202" ,"1700116707", "1705905102" ,"14002024XX", "14108111XX",
          "14132008XX","1411803003" ,"17059051XX")

alpha <- c("PPG" ,"NIP" ,"TLN" ,"RHI" ,"CAN", "CSY", "CLZ", "TLP")

fish_scientific <-  tolower(c("Protopterus annectens", "Lates niloticus" ,
                              "Oreochromis niloticus", "Labeo spp",   "Bagrus spp"  ,
                              "Synodontis spp", "Clarias gariepinus" ,"Oreochromis spp" ))

for(i in 1:8){
  
  id2 <- "1503"
  
  n1 <- dim(dictionary.df)[1]+i
  
  n2 <- which(dictionary.df$ID_2 %in% id2)
  
  dictionary.df[n1,] <- dictionary.df[n2,]
  
  dictionary.df[n1,7] <- paste0(id2, ".0", i)
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(fish_name[i], ", fillet, fresh, raw")
  dictionary.df[n1,10] <- paste0("ISSCAAP Code: ", isscaap[i],"; Taxonomic Code: ", taxo[i], "; Inter-Agency3-Alpha Code: ", alpha[i])
  dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
  dictionary.df[n1,13] <- fish_scientific[i]
  
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
dictionary.df[n1,12] <- "10.48580/dfpk-37v"
dictionary.df[n1,13] <- "sardinella spp."

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

# Upsidedown catfishes (sometimes called catfish)

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1501.07")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1501.09"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "upsidedown catfishes, fresh, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:13; Taxonomic Code:14132008XX; Inter-Agency3-Alpha Code:CSY"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/08). (2022)"
dictionary.df[n1,12] <- "also called catfish"
dictionary.df[n1,13] <- "synodontis spp."

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

############ End fish ##################

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


#Sardines

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1527.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1527.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sardines, fresh, raw"
dictionary.df[n1,10] <- "ISSCAAP Code:35; Taxonomic Code:12105012XX; Inter-Agency3-Alpha Code:SIX"
dictionary.df[n1,11] <- "FAO-FIES. Aquatic Sciences and Fisheries Information System (ASFIS) species list. Retrievef from http://www.fao.org/fishery/collection/asfis/en (accessed 2022/08/01). (2022)"
dictionary.df[n1,12] <- "10.48580/dfpk-37v"
dictionary.df[n1,13] <- "sardinella spp."


#Prawns

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1553.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1553.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "prawns, fresh, raw"
dictionary.df[n1,13] <- "penaeidae"


#Cream

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "22120.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "22120.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cream, cow milk, 35%"
dictionary.df[n1,13] <- NA

#Chicken
which(dictionary.df$ID_3 == "21121.01")

dictionary.df[643,] <- dictionary.df[440,]

dictionary.df[643,7] <- "21121.01.01"
dictionary.df[643,8] <- NA
dictionary.df[643,9] <- "chicken meat, fresh, meat, skin, without bones, raw"

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


#Broad bean
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1702.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1702.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "broad bean, dried, raw"
dictionary.df[n1,13] <- "vicia faba"

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

# Soya milk

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


### Fruits and Vegetables (FV) ----

#Leaves can be fresh or dried. 
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.01"] <- "spinach, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.02"] <- "amaranth leaves, fresh, raw"

#Exist another code for onions
#01253.01 - Onions, shallots, green


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

dictionary.df[n1,7] <- "21346.01"
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

#Jackfruit
n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "1319")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1319.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "jackfruit, fresh, raw"
dictionary.df[n1,10] <- "Fruit, tropical fresh nes (603)"
dictionary.df[n1,11] <- "FAO (2022). Supply Utilization Accounts. Accessed last on 2022-07-22. https://www.fao.org/faostat/en/#data/SCL"
dictionary.df[n1,13] <- "artocarpus heterophyllus"

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

# Malabar Spinach
subset(dictionary.df, ID_2 == "1290.9")

#Manual inputs:
id2 <- "1290.9"
desc_new <- "malabar spinach, leaves, fresh, raw"
fex2_new <- NA
scien_new <- "basella alba"
#(optional)
desc2 <- "also called Vine (African) spinach"
ref2 <-  "KE18, and https://www.fondazioneslowfood.com/en/ark-of-taste-slow-food/nderema/#:~:text=Nderema%2C%20also%20known%20as%20vine,green%20or%20brownish%2Dpurple%20stems."

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,10] <- desc2
dictionary.df[n1,11] <- ref2
dictionary.df[n1,13] <- scien_new

# Spider plant

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1290.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.07"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "spider plant, leaves, fresh, raw"
dictionary.df[n1,13] <- "gynandropsis gynandra"


# Black Nightshade

#Manual inputs:
id2 <- "1290.9"
desc_new <- "black nightshade, leaves, fresh, raw"
fex2_new <- NA
scien_new <- "solanum scabrum"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

#Native eggplant

#Manual inputs:
id2 <- "1290.9"
desc_new <- "native eggplant, raw"
fex2_new <- NA
scien_new <- "solanum macrocarpon"

#Auto inputs:
id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                 paste0( str_extract(id3, 
                                     "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                         as.numeric(str_extract(id3, "[[:digit:]]$"))+1))

n1 <- dim(dictionary.df)[1]+1

n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
             which(dictionary.df$ID_3 %in% id3))

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- id3_new
dictionary.df[n1,8] <- fex2_new
dictionary.df[n1,9] <- desc_new
dictionary.df[n1,13] <- scien_new

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

#Taro leaves

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1241.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.06"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "taro leaves, fresh, raw"
dictionary.df[n1,13] <- "colocasia esculenta"

#Cow peas leaves

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1241.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.05"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cow pea leaves, fresh, raw"
dictionary.df[n1,13] <- "vigna unguiculata"

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


#Purple passion fruit

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1319.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1319.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "passion fruit, dark purple skin, fresh, raw"
dictionary.df[n1,13] <- "passiflora edulis"


#Sweet potato leaves

#Manual inputs:
id2 <- "1290.9"
desc_new <- "sweet potato leaves, fresh, raw"
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


#Coriander leaves
#Note that in KE18 the food group is "condiments", also in FAO-SUA there is a 
#mention to coriander seeds under spices. 

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1290.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "coriander leaves, fresh, raw"
dictionary.df[n1,13] <- "coriandrum sativum"

#Celery

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1290.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.04"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "celery, fresh, raw"
dictionary.df[n1,13] <- "apium graveolens"

#Courgette

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1235.03")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1235.04"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "courgette, green, fresh, raw"
dictionary.df[n1,10] <- "also called zucchini, baby marrow, summer squash"
dictionary.df[n1,13] <- "cucurbita pepo var. Cylindrica"

#Add - 1290.9 - radish, fresh, raw
#Manual inputs:
id2 <- "1290.9"
desc_new <- "radish, fresh, raw"
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

########## Other foods (OT) ##############
subset(dictionary.df, ID_1 == "2659")

#Correcting category codes & description (ID_0 & FoodName_0)
#"2581"|ricebran oil and products                  
#"2582"|maize germ oil and products                
#"2657"|beverages, fermented and products          

dictionary.df$ID_0[dictionary.df$ID_1 %in% c("2659","2581", "2582",
                                             "2657") ] <- "OT"
dictionary.df$FoodName_0[dictionary.df$ID_1 %in% c("2659","2581", "2582",
                                                   "2657")] <- "Other foods"
#Correcting food description by codes
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1460.01"] <- "coconut, mature, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23670.01.01"] <- "sweets, including chewing gum"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23511.02.01"] <- "sugar, from unrefined sugar cane"
dictionary.df$Description2[dictionary.df$ID_3 == "23511.02.01"] <- "jaggery, panela, raw sugar"
dictionary.df$Desc1.ref2[dictionary.df$ID_3 == "23511.02.01"] <- "https://doi.org/10.1016/j.foodchem.2017.01.134"

#Correcting food item classification: 1- food item code, 2- sub-classification
subset(dictionary.df, ID_3 == "F1232.07", select = c(1:6))
subset(dictionary.df, ID_2 == "F1232", select = c(1:6))
unique(subset(dictionary.df, ID_2 == "F1232", select = c(1:6)))
#Amend yeast (1699.04) --> F1232.06
dictionary.df$ID_3[dictionary.df$ID_3 == "1699.04"] <-  "F1232.06"
dictionary.df[which(dictionary.df$ID_3 == "F1232.06"), c(1:6)] <- unique(subset(dictionary.df, ID_2 == "F1232", select = c(1:6)))
#Amend baking powder (1699.05) --> F1232.07
dictionary.df$ID_3[dictionary.df$ID_3 == "1699.05"] <- "F1232.07"
dictionary.df[which(dictionary.df$ID_3 == "F1232.07"), c(1:6)] <- unique(subset(dictionary.df, ID_2 == "F1232", select = c(1:6)))
#Amend tabasco sauce (1699.06)  --> F1232.08
dictionary.df$ID_3[dictionary.df$ID_3 == "1699.06"] <- "F1232.08"
dictionary.df[which(dictionary.df$ID_3 == "F1232.08"), c(1:6)] <- unique(subset(dictionary.df, ID_2 == "F1232", select = c(1:6)))


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

#Vegetable fat

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 == "F1243")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F1243.01"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "vegetable, fat"
dictionary.df[n1,13] <- NA

#├ New item (ID_3) ----

#sauces and spice

foods <- c("mayonnaise", "soup", "potash", "chilli sauce", "maize porridge")

for(i in 1:length(foods)){
  
  id2 <- "F1232"
  
  n1 <- dim(dictionary.df)[1]+i
  
  n2 <- which(dictionary.df$ID_2 %in% id2)
  
  dictionary.df[n1,] <- dictionary.df[n2,]
  dictionary.df[n1,7] <- paste0(id2, ".0", i)
  dictionary.df[n1,8] <- NA
  dictionary.df[n1,9] <- paste0(foods[i])
  
}

#candies, hard 

#Manual inputs:
id2 <- "23670.01"
desc_new <- "candies, hard"
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

#Tea with milk

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "23914.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "23914.03"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "tea with milk"

#Curry powder

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1699.03")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1699.10"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "curry powder"

#Coconut, inmature

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1460.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1460.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "coconut, inmature, fresh, raw"
dictionary.df[n1,13] <- "cocos nucifera"

#Animal fat

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "F1243.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "F1243.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "Animal, fat"
dictionary.df[n1,13] <- NA

#infant formula, 3 months, fortified

id3 <- "23991.01.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "infant formula, 3 months, fortified"

#infant formula, 6 months, fortified

id3 <- "23991.01.03"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 %in% id3)

dictionary.df[n1,] <- dictionary.df[n2,]


dictionary.df[n1,7] <- paste0( str_extract(id3, 
                                           "[[:alnum:]]{2,5}\\.\\d{1,2}\\.\\d{1}|[[:alnum:]]{2,5}\\.\\d{1}"),
                               as.numeric(str_extract(id3, "[[:digit:]]$"))+1)
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "infant formula, 6 months, fortified"

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

########## Roots and Tubers (RT) ##############


#Cassava dried

id2 <- "1520.02"

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_2 %in% id2)

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- paste0(id2, ".01")
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "cassava, root, dried, raw"
dictionary.df[n1,13] <- "manihot esculenta"

#├ New item (ID_3) ----

#Changing name cocyam and adding scientific name (1591.01)
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1591.01"] <- "cocoyam, white, fresh, raw"
dictionary.df$Description1[dictionary.df$ID_3 == "1591.01"] <- "Also called tannias, and yautia"
dictionary.df$Description1[dictionary.df$ID_3 == "1591.01"] <- "xanthosoma sagittifolium"

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


# Final Formatting ----

dictionary.df$FoodName_2 <- str_squish(dictionary.df$FoodName_2)

dictionary.df <- dictionary.df %>% arrange(.)



#Save an R object - Running MAPS_dict_QC.R
saveRDS(dictionary.df, file = here::here("inter-output", "dictionary.df.rds"))

