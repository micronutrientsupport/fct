
################################################################################
#                                                                              #
#                                                                              #
#             Malawi Food Composition Table (MAFOODS, 2019)                    #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Library loading 

library(tidyverse)

# Data Import ----

readxl::read_excel(here::here("FCTs", 'MW19', 
                              'MMFDB-Unprotected-2.xlsx'), sheet = 2) 

mwfct <- readxl::read_excel(here::here("FCTs", 'MW19', 'MMFDB-Unprotected-2.xlsx'), sheet = 1) %>%  #Reads the excel document and assigns the relevant sheet to an R data frame
  mutate(source_fct = 'MW19')  #Creates the source_fct column and fills it with "WA19_FCT"


# Renaming variable names

# Changing the variable names to the FAO tagnames

FCT_tag <- c("fdc_id",'nutrient_data_source', "food_desc", 'food_group',  'WATERg',
             'ENERCkcal', 'ENERCkJ',
             'NTg', 'PROCNTg', 'FATg', 'FASATg', 'FAMSg', 'FAPUg', 'CHOLEmg', 
             'CHOCSMg',
             'CHOAVLDFg','SUGARg', 'SUGADg', 'FIBCg', 'STARCHg', 'ASHg', 'CAmg',
             'FEmg', 
             'MGmg', 'Pmg', 'Kmg', 'NAmg', 'ZNmg', 'CUmg', 'MNmcg', 'IDmcg', 'SEmcg',
             'VITA_RAEmcg',
             'VITAmcg', 'THIAmg', 'RIBFmg', 'NIAmg', 'VITB6_mg', 'FOLmcg', 
             'VITB12mcg', 'PANTACmg', 'BIOTmcg', 'VITCmg', 'VITDmcg', 'VITEmg' ,
             'PHYTmg', "source_fct")


mwfct <- mwfct %>% rename_all( ~ FCT_tag)

# Removing special characters

# 4) Removing [] and ()

#The following f(x) removes [] and ()

no_brackets <- function(i){
  case_when(
    str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    str_detect(i, '\\(.*?\\)') == TRUE ~ str_extract(i, '(?<=\\().*?(?=\\))'),
    TRUE ~ i)
}


# removing [] and () and converting nutrient variables (nut) into numeric

mwfct[, c(5:46)] <- apply(mwfct[, c(5:46)], 2, no_brackets)
mwfct[, c(5:46)] <- apply(mwfct[, c(5:46)], 2, as.numeric)


# 5) Changing mineral values in ref.10 to reconverted values

#Filtering data entries in MAFOODS from Joy et al. paper

mwi_water <- mwfct %>% dplyr::filter(nutrient_data_source == "10") %>% 
  arrange(fdc_id) %>% pull(fdc_id) 


EJ <- read.csv(here::here('data',
                          'mineral-composition_2020-11-06.csv')) %>% 
  select(-contains('median'))


#Changing mineral values on the data set (see documentation)

mw19_ej <- mwfct %>% dplyr::filter(fdc_id %in% mwi_water) %>% 
  inner_join(., EJ, by = c('fdc_id' = 'water_ref')) %>% 
  mutate(
    CAmg = ca_mg_100g, 
    CUmg = cu_mg_100g, 
    FEmg = fe_mg_100g, 
    MGmg = mg_mg_100g,
    SEmcg = se_mcg_100g, 
    ZNmg = zn_mg_100g) %>% 
  select(1:47)

#Substituting old (incorrect) values to new values

mwfct <- mwfct %>% dplyr::filter(!fdc_id %in% mwi_water) %>% 
  bind_rows(., mw19_ej)

#Optional - check the data before saving
glimpse(mwfct)

# Data Output ----

write.csv(mwfct, file = here::here("FCTs", "MW19_FCT_FAO_Tags.csv"), 
          row.names = FALSE) #Saves the newly-created data table to the Output folder

#Run this to clean the environment
rm(list = ls())
