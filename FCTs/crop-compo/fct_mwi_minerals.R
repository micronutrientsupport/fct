
################################################################################
#                                                                              #
#                 Malawi                                                       #                          
#          Crop Composition Data from Joy et al (2015)                         #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Library loading 

library(dplyr)
library(visdat)

# 0) Accessing the data (for source of the data see README) 

# Only need to do it the first time to obtain the raw files!
# If doesn't find the excel file then it download it, otherwise skips it.

if(sum(is.na(list.files(here::here("FCTs", "crop-compo"), pattern = ".xlsx"))) == 0){

# Where to download
  f <- "https://ars.els-cdn.com/content/image/1-s2.0-S0048969714014764-mmc1.xlsx"
  
  # Downloading and saving it
download.file(f, 
             destfile = here::here("FCTs", 'crop-compo', "2015_Joy_crop-min-composition.xlsx"),
             method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
            mode="wb")

}

# Cleaning the enviroment
rm(list = ls())

# Loading data on mineral micronutrient
# Checking the sheets
readxl::excel_sheets(here::here('FCTs', "crop-compo", 
                              '2015_Joy_crop-min-composition.xlsx'))
# Loading the compo data
data.df <- readxl::read_excel(here::here('FCTs', "crop-compo", 
                                            '2015_Joy_crop-min-composition.xlsx'), 
                                 sheet = 'STable7', skip = 2)

# Getting other FCTs for the water content
source(here::here("MAPS_fct_load.R"))

# Adding missing mineral (Se from grain maize) ----
## Extracting ratio ----

## Maize to flour ration (Supl.Table6, Joy et al., 2015)
ratio <- readxl::read_excel(here::here("FCTs", "crop-compo",
                                       "40795_2015_36_MOESM1_ESM.xlsx"), 
                            sheet = 6, skip = 2)
head(ratio)

# Only need column 1 (element) and column 7 ratio
ratio <- ratio %>% select(c(1, 7)) %>%
  filter(!is.na(Element)) %>% 
  rename(ratio_refine="Mean ratio refined flour:whole grain") %>% 
  mutate(crop = "maize")

head(ratio)

# Re-name variables
# All varibales are in mg/Kg DW
mwi_mn <- data.df %>% rename(food_desc = '...1', 
                                foodtissue = '...2', 
                                foodnotes = '...3', 
                                soiltype = '...4', 
                                n_sample = 'Ca',
                                ca_median = '...9',
                                cu_median = '...18',
                                fe_median = '...27', 
                                mg_median = '...36',
                                se_median = '...45',
                                zn_median = '...54') 


# Filtering items with combined data and more than 1 sample for representativeness purposes

mwi_mn <-  mwi_mn %>% 
  filter(soiltype == 'Combined', n_sample != "1") %>% 
  select(!starts_with('...'))

#Converting variables into numeric

mn <- c('ca', 'cu', 'fe', 'mg', 'se', 'zn')

mwi_mn <-  mwi_mn %>%
  mutate_at(vars(starts_with(mn)), as.numeric)  %>% 
  mutate(source_fct = "Joy et al, 2015", 
         fdc_id = paste0("EJ15_",row.names(mwi_mn)))


# ToDO: Amending codes
#Se.df$water_ref[Se.df$water_ref == "04_110"] <- "04_004"

#Adding water values for conversion and reference
mwi_mn <-  mwi_mn %>% mutate(
  water = c(87.8, 
            9,
            72.0,
            83.8, #Median, MAFOODS leaves
            11, #bean, seed
            88.0, 
            84.0, 
            92.2,
            95.0, 
            73.2,
            59.7,
            83.8, #Median, MAFOODS leaves
            11, #bean, seed 
            87.1, #Cowpea WAFCT
            86.3, #Cowpea WAFCT (not dried)
            11.1, 
            NA,
            90.0, 
            8.7, 
            11.3, 
            NA,
            6.5, 
            83.0, 
            NA, 
            83.6,
            77.3,
            10.9, 
            10.0, 
            10.0,
            83.5,
            85.2,
            86.7,
            75.1,
            NA, 
            NA, 
            10.5 , 
            91.55, 
            86.99,
            NA, 
            84.0, 
            84.3,
            89.1,
            88.1, 
            78.2, 
            9.4,
            9.1,
            63.0,
            8.0,
            92.9,
            5.5, 
            89.7, 
            13.0,
            12.4,
            8.5,
            90.0, 
            5.9,
            83, 
            77.3, 
            NA,
            94.5,
            NA,
            64.7),
  water_ref = c('MW04_0011',
                '03_001', #WAFCT
                'MW05_0004', 
                'MW04_0002;MW04_0010;MW04_0013;MW04_0014;MW04_0015;MW04_0016;MW04_0017;MW04_0018;MW04_0021;MW04_0033;MW04_0011;MW04_0012;MW04_0019;MW04_0020', 
                'MW02_0004', #bean, seed
                'MW04_0012', 
                'MW04_0010', #amaranth 
                'MW04_0004', 
                'MW04_0003', #not dried 
                'MW04_0014', 
                'MW01_0011', 
                'MW04_0002;MW04_0010;MW04_0013;MW04_0014;MW04_0015;MW04_0016;MW04_0017;MW04_0018;MW04_0021;MW04_0033;MW04_0011;MW04_0012;MW04_0019;MW04_0020', 
                'MW02_0004', #common bean, seed (13)
                '04_010', #cowpea leaves WAFCT
                '04_098', #cowpea leaves (not dried) boiled
                'MW02_0007',
                'missing data', #Dorica bean
                'MW04_0007', 
                'MW01_0016', 
                '1026', #finger millet flour KENFCT
                'missing data', #Gondorosi (root)
                'MW02_0014', #groundnut dry
                'MW05_0008',
                'missing data', #hibiscus flower
                'MW04_0021',
                'MW04_0016',
                 'MW01_0037', 
                'MW01_0020',
                'MW01_0020', #review - seems different
                'MW05_0016',
                'MW05_0013',
                'MW05_0015',
                '04_011', #moringa leave WAFCT
                'missing data', #moringa pod
                'missing data', #moringa seed
                'WA19(03_059)', #mucuna = velvet (dried)
                'MW04_0027', # Mushroom
                'median mustard leave in LSOFCT',
                'missing data', #Ntumwa
                'MW04_0030',
                '04_110', #okra leaves in WAFCT
                'MW04_0031',
                'MW05_0019',
                'MW04_0032',  #peas raw (fresh)
                '01_017', #pearl millet grain (whole)
                '01_063', #pearl millet flour (w/o bran)
                'MW02_0018', #pigeon pea, green, boiled
                'MW02_0017',
                'MW04_0019',
                '06_038', #pumpkin seed, kernel only, dried
                'MW04_0020',
                'MW01_0058',
                'MW01_0060',
                'MW02_0019',
                'MW08_0007',
                '10015', #sunflower seed in KENFCT
                '04_059', #sweet potato leaves in WAFCT
                'MW01_0065', #white-fleshed 
                'missing data', #tea leaf
                'MW04_0036',
                'missing data', #Tove leaves
                'MW01_0013') #cocoyam
)

# Added this to the foodnotes bc the water conversion is going to use 
# the bean, dried.
mwi_mn$foodnotes[mwi_mn$food_desc == "Mucuna"] <- "dried"
mwi_mn$water[mwi_mn$water_ref == "MW04_0027"] <-  91.6 


# Adding water to tea leaves
mwi_mn$water[mwi_mn$fdc_id == "EJ15_59"] <- 9.30
mwi_mn$water_ref[mwi_mn$fdc_id == "EJ15_59"] <- "DK19(537)"  

# Finding water content
fct_dict %>% 
  filter(grepl("tea |tea,", food_desc, ignore.case = TRUE), 
         grepl("leav", food_desc, ignore.case = TRUE)) %>% 
  select(source_fct, fdc_id, food_desc, WATERg)

mwi_mn$comments <- NA
#Adding refined flour
mwi_mn[nrow(mwi_mn)+1, ] <- mwi_mn[mwi_mn$water_ref == "MW01_0037", ] 
mwi_mn[nrow(mwi_mn), c("foodnotes")] <- "Flour, refined (mgaiwa)"
mwi_mn[nrow(mwi_mn), c("water_ref")] <- "MW01_0019"
mwi_mn[nrow(mwi_mn), c("water")] <- 12.3
mwi_mn[nrow(mwi_mn), c("comments")] <- "Values calculated from Maize Grain (EJ)"

#Adding  bran flour 
mwi_mn[nrow(mwi_mn)+1, ] <- mwi_mn[mwi_mn$water_ref == "MW01_0037", ] 
mwi_mn[nrow(mwi_mn), c("foodnotes")] <- "Flour, bran (madeya)"
mwi_mn[nrow(mwi_mn), c("water_ref")] <- "MW01_0020"
mwi_mn[nrow(mwi_mn), c("water")] <- 10
mwi_mn[nrow(mwi_mn), c("comments")] <- "Values calculated from Maize Grain (EJ) and 1-ratio"


# Changed ratio of Fe to 1 bc can't be higher than 1
ratio$ratio_refine[ratio$Element== "Fe"] <- 1
ratio <- subset(ratio, Element != "I") # Removed I bc not reported

for(i in 1:nrow(ratio)){
variable <- paste0(tolower(ratio$Element[i]), "_median")

value <- mwi_mn[nrow(mwi_mn),  c(variable)]*(1-ratio$ratio_refine[i])

mwi_mn[nrow(mwi_mn),  c(variable)] <- value

}

#Adding green maize
mwi_mn[nrow(mwi_mn)+1, ] <- mwi_mn[mwi_mn$water_ref == "MW01_0037", ] 
mwi_mn[nrow(mwi_mn), c("foodnotes")] <- "green"
mwi_mn[nrow(mwi_mn), c("water_ref")] <- "MW01_0040"
mwi_mn[nrow(mwi_mn), c("water")] <- 68
mwi_mn[nrow(mwi_mn), c("comments")] <- "Values calculated from Maize Grain (EJ)"


# Calculation of averaged water values used above

# 1) median all leaves in MAFOODS

fct_dict %>%  
  dplyr::filter(source_fct == 'MW19') %>% 
  dplyr::filter(grepl("leave", food_desc, 
                      ignore.case = TRUE))  %>%
  summarise(median(as.numeric(WATERg), na.rm = TRUE), 
            fdc_id = paste0(fdc_id, collapse = ";")) %>% 
  pull(fdc_id)


# 2) median all mushroom raw in MAFOODS (depri)

fct_dict %>%  
  dplyr::filter(source_fct == 'MW19') %>% 
  dplyr::filter(grepl("mushroom", food_desc, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("raw", food_desc, 
                      ignore.case = T))  %>%
  summarise(median(as.numeric(WATERg), na.rm = TRUE))

#3) median mustard leave in LSOFCT

fct_dict %>%  
  dplyr::filter(grepl("mustard", food_desc, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("leave", food_desc, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("raw", food_desc, 
                      ignore.case = T))  %>%
  summarise(median(as.numeric(WATERg), na.rm = TRUE))


fct_dict %>%  
  dplyr::filter(grepl("mucuna|vet", food_desc, 
                      ignore.case = TRUE)) %>% 
  select(fdc_id, food_desc, scientific_name, WATERg)

# Adding woyera


## Standardising units ----

#Create a function to convert from dry (mg/Kg == mcg/g) to wet weight (mcg/100g)
#x = dry weight in mg/Kg
#y = water g/100g 
#mn = nutrient in mcg/100g wet weight
# As per FAO/ INFOODS Guidelines for Converting Units, Denominators 
# and Expressions Version 1.0, page 12

dry_wet <- function(x, y){
  
  mn <- as.numeric(x) * (100 - y)
  
  return(mn)
  
}

# Converting the min from dry to wet weight

mwi <-  mwi_mn %>%
  # Transforming into numeric
  mutate_at(vars(matches('_median')), as.numeric) %>% 
   # Transforming to wet weight using the water content in 100g
  mutate(across(ends_with('_median'),
                   ~dry_wet(.x, mwi_mn$water))) %>%
   # Renaming to to reflect the WW in 100g
  rename_at(vars(matches('_median')),
    ~stringr::str_replace_all(., 'median', 'mcg_100g')) %>% 
  # Changing units
  mutate(across(grep("ca|mg|fe|cu|zn", names(.),  value =TRUE), ~.x/1000)) %>% 
  # Renaming to to reflect the new units in 100g
  rename_at(vars(grep("ca|mg|fe|cu|zn", names(.),  value =TRUE)),
            ~stringr::str_replace_all(., 'mcg', 'mg')) %>% 
   # Selecting the variables needed
  select(fdc_id, food_desc, foodtissue, foodnotes, n_sample, 
         contains('_100g'), water, water_ref, source_fct)  %>% 
  # ToDo: Change units of other Minerals
   mutate(
          foodnotes = ifelse(is.na(foodnotes), "raw", tolower(foodnotes)), 
          food_item = paste(food_desc, tolower(foodtissue), foodnotes, sep = ",")) %>% 
   select(-c( foodtissue, foodnotes)) 
 
mwi %>% 
   right_join(., mwi_mn %>% 
                filter(soiltype == 'Combined', n_sample != "1") ) %>% 
write.csv(., here::here('data',
                              'mineral-composition_2024-10-14.csv'))






# Generating a standardised version
names(mwi)

mwi <- mwi %>% rename(
  WATERg = "water", 
  CAmg = "ca_mg_100g",
  MGmg  = "mg_mg_100g",
  FEmg  = "fe_mg_100g", 
  CUmg  = "cu_mg_100g", 
  ZNmg  = "zn_mg_100g", 
  SEmcg = "se_mcg_100g",
 # comments = "water_ref"
) %>% 
  mutate(
    # source_fct = "Joy et al, 2015", 
    #   fdc_id = paste0("EJ15_",row.names(mwi_mn)), 
         comments = paste0("water ref. (", water_ref, "); samples (n=", n_sample, ")")) %>% 
  relocate(SEmcg, .after = "ZNmg") %>% 
  relocate(WATERg, .before = "CAmg") %>% 
  relocate(fdc_id, .before = "food_desc") 





#  mwi_mn[, nrow(mwi_mn)+1] <- mwi_mn[mwi_mn$fdc_id == "EJ15_27", ]
#Adding refined flour & bran flour
# refined <- mwi_mn$SEmcg[mwi_mn$fdc_id == "EJ15_27" & !is.na(mwi_mn$SEmcg)]*ratio$ratio_refine[ratio$Element == "Se"]
# bran <- mwi_mn$SEmcg[mwi_mn$fdc_id == "EJ15_27" & !is.na(mwi_mn$SEmcg)]*(1-ratio$ratio_refine[ratio$Element == "Se"])
# 
# mwi_mn[nrow(mwi_mn)+1, ] <- NA
# mwi_mn[nrow(mwi_mn), c("fdc_id")] <- paste0("EJ15_", nrow(mwi_mn)+1)
# mwi_mn[nrow(mwi_mn), c("food_desc")] <- c("Maize, Grain, Flour, refined")
# mwi_mn[nrow(mwi_mn), "SEmcg"] <-  refined
# mwi_mn[nrow(mwi_mn)+1, ] <- NA
# mwi_mn[nrow(mwi_mn), c("fdc_id") ] <- paste0("EJ15_", nrow(mwi_mn)+1)
# mwi_mn[nrow(mwi_mn), "SEmcg" ] <-  bran
# 

mwi %>% # select(comments)
  write.csv(., here::here("FCTs", "crop-compo_FCT_FAO_Tags.csv"), 
            row.names = FALSE)

#clean environment
rm(list = ls())

# Selecting only variables of interest in MAFOODS (minerals, water, names and ref)
#
#mwi_check <-  mwi_check %>% 
#  select(code, fooditem, ref, CA, CU, 
#                                   FE, MG, SE, ZN, WATER)
#
##Pasting together MAFOODS data (from Edward's paper) and 
##Data calculated from actual data (from Edward's paper)
#
#
#mwi_check <- mwi_check %>% 
#  right_join(., mwi_mn, by= c('code' = 'water_ref'),
#             suffix('_MAFOODS', '_Edward')) #names for the variable fooditem
#
#write.csv(mwi_check,
#          here::here('data', 'mineral-composition.comparison_2020-11-02.csv'))
#
#
#
#water_adjustment <- function(x, y){
#  
#  mn <- mn * (100 - y)/(100 - x)
#  
#  mn
#  
#}
#
##Functions
#
#water_adjustment <- function(y) {
#  
#  x <- y * (100-WATER.x)/(100-WATER.y)
#  
#  x
#  
#}
#
## fct <- read.csv(here::here('data', 'FCT_06.csv'))
#
#
#
##Filtering data entries in MAFOODS from Edward's paper
#
#mwi_check <- fct %>% dplyr::filter(FCT == "MAFOODS", ref == "10")
#
#