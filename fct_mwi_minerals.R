

library(tidyverse)


fct <- read.csv(here::here('data', 'FCT_06.csv'))

#Filtering data entries in MAFOODS from Edward's paper

mwi_check <- fct %>% dplyr::filter(FCT == "MAFOODS", ref == "10")

#Loading data on mineral micronutrient calculated by Edward

mwi_mn_raw <- readxl::read_excel(here::here('data',
                    '2015_Joy_crop-min-composition.xlsx'), 
                     sheet = 'STable7', skip = 2)


mwi_mn <- mwi_mn_raw %>% rename(fooditem = '...1', 
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



mwi_mn <-  mwi_mn %>% 
  filter(soiltype == 'Combined', n_sample != "1") %>% 
  select(!starts_with('...'))

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
            NA, 
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
                'median all leaves in MAFOODS', 
                'MW02_0004', #bean, seed
                'MW04_0012', 
                'MW04_0010', #amaranth 
                'MW04_0004', 
                'MW04_0003', #not dried 
                'MW04_0014', 
                'MW01_0011', 
                'median all leaves in MAFOODS', 
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
                'missing data', #mucuna
                'median all mushroom raw in MAFOODS',
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




fct %>%  
  dplyr::filter(FCT == 'MAFOODS') %>% 
  dplyr::filter(grepl("leave", fooditem, 
                      ignore.case = T))  %>%
  summarise(median(WATER))

fct %>%  
  dplyr::filter(FCT == 'MAFOODS') %>% 
  dplyr::filter(grepl("leave", fooditem, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("boil", fooditem, 
                      ignore.case = T))  %>%
  summarise(median(WATER))

#median all mushroom raw in MAFOODS

fct %>%  
  dplyr::filter(FCT == 'MAFOODS') %>% 
  dplyr::filter(grepl("mushroom", fooditem, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("raw", fooditem, 
                      ignore.case = T))  %>%
  summarise(median(WATER))

#median mustard leave in LSOFCT

fct %>%  
  dplyr::filter(grepl("mustard", fooditem, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("leave", fooditem, 
                      ignore.case = T))  %>%
  dplyr::filter(grepl("raw", fooditem, 
                      ignore.case = T))  %>%
  summarise(median(WATER))

#Create a function to convert from dry (mg/Kg) to wet weight (mg/100g)
#x = dry weight in mg/Kg
#y = water g/100g 
#mn = nutrient in mg/100g wet weight


dry_wet <- function(x, y){
  
  mn <- x/10
  
  mn <- mn * (100 - y)/100
  
  mn
  
}


#a <- mwi_mn %>% mutate(
#  FE_100g =  dry_wet(fe_median, water))

for (lag_size in c('ca', 'cu', 'fe', 'mg', 'se', 'zn')) {
  new_col_name <- paste0(lag_size, "_100g")
  old_col_name <- paste0(lag_size, "_median")
  
  a <- a %>% 
    mutate(!!sym(new_col_name) := dry_wet(x = old_col_name,
                                          y = water, na.rm = TRUE))
}

a <- mwi_mn %>% mutate_at(vars(matches('_median')), as.numeric)

x <- 'ca'

a <- a %>% mutate(
  ca_100g =  dry_wet(paste0(x, '_median'), water))

a <- a %>% 
  mutate(across(matches('_median'),
                   ~dry_wet(.x, a$water)))

a <- a %>%
  rename_at(vars(matches('_median')),
    funs(
        stringr::str_replace_all(., 'median', 'mg_100g'))) 

a <- a %>% 
  select(fooditem, foodtissue, foodnotes, soiltype, n_sample, 
         contains('mg_100g'), water, water_ref)

mwi_mn <- mwi_mn %>% left_join(., a)

mwi_mn <-  mwi_mn %>% 
  select(-c(Cu, Fe, Mg, Se, Zn)) %>% 
 mutate(
  se_mcg_100g = se_mg_100g*1000) %>% 
  select(-se_mg_100g)


write.csv(mwi_mn, here::here('data',
                              'mineral-composition_2020-11-06.csv'))

#Selecting only variables of interest in MAFOODS (minerals, water, names and ref)

mwi_check <-  mwi_check %>% 
  select(code, fooditem, ref, CA, CU, 
                                   FE, MG, SE, ZN, WATER)

#Pasting together MAFOODS data (from Edward's paper) and 
#Data calculated from actual data (from Edward's paper)


mwi_check <- mwi_check %>% 
  right_join(., mwi_mn, by= c('code' = 'water_ref'),
             suffix('_MAFOODS', '_Edward')) #names for the variable fooditem

write.csv(mwi_check,
          here::here('data', 'mineral-composition.comparison_2020-11-02.csv'))