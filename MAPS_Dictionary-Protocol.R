
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

dictionary.df <- read.csv(here::here("metadata", "MAPS_Dictionary_v2.6.csv")) %>% 
  select(-starts_with("X"))

dictionary.df$scientific_name <- NA

colnames(dictionary.df)

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
      FoodName_3 = " palm kernel, raw") %>% 
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

#PB Pulses and Beans 2561             sesame seed and products
#PB Pulses and Beans 2571            soyabean oil and products
#PB Pulses and Beans 2572           groundnut oil and products
#PB Pulses and Beans 2579          sesameseed oil and products

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
    ID_2 = "1251", 
    FoodName_2 =  "carrots and turnips", 
    ID_3 = "1251.01",
    FE2_3 = "",
    FoodName_3 = "carrot, raw")  %>% 
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


#├ New item (ID_3) ----

#Adding description
dictionary.df$Description1[dictionary.df$ID_3 == "23710.01"] <- "default pasta (spaghetti)"

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


### Animal products (AP) ----

#Correcting codes
dictionary.df$ID_2[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"] <- "F1061"
dictionary.df$ID_3[dictionary.df$FoodName_3 == "chicken meat, fresh, deep-fried"]  <- "F1061.01"              

#├ New category from ID_2 ----

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


#├ New item (ID_3) ----

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

### Pulses and Beans (PB) ----

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


### Fruits and Vegetables (FV) ----

#Leaves can be fresh or dried. 
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.01"] <- "spinach, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1215.02"] <- "amaranth leaves, fresh, raw"

#Exist another code for onions
#01253.01 - Onions, shallots, green

#├ New category from ID_2 ----

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
dictionary.df[n1,13] <- "pyrus communis "


#├ New item (ID_3) ----

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

n1 <- dim(dictionary.df)[1]+1

n2 <- which(dictionary.df$ID_3 == "1290.9.01")

dictionary.df[n1,] <- dictionary.df[n2,]

dictionary.df[n1,7] <- "1290.9.02"
dictionary.df[n1,8] <- NA
dictionary.df[n1,9] <- "sweet potato leaves, fresh, raw"

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


########## Other foods (OT) ##############

#Correcting codes
dictionary.df$FoodName_3[dictionary.df$ID_3 == "1460.01"] <- "coconut, mature, fresh, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23511.02.01"] <- "sugar, from unrefined sugar cane"
dictionary.df$Description2[dictionary.df$ID_3 == "23511.02.01"] <- "jaggery, panela, raw sugar"
dictionary.df$Desc1.ref2[dictionary.df$ID_3 == "23511.02.01"] <- "https://doi.org/10.1016/j.foodchem.2017.01.134"

#├ New category from ID_2 ----

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






#Run this to over-write any new upgrades in adding new food dictionary codes
#in dictionary folder


#IMPORTANT: to keep all folder updated run this code!!
#Saving Food-Dictionary into all r-project that use it

#here we are checking where the previous version was stored:
#
rproject.path <- "C:/Users/LuciaSegoviaDeLaRevi/OneDrive - London School of Hygiene and Tropical Medicine/MAPS/02_working-files/r-project"

#knowing all the MAPS_Dictionary-Protocol.R

x  <- list.files(rproject.path, 
           pattern = "MAPS_Dictionary-Protocol.R",
           recursive = TRUE)
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