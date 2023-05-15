################################################################################
#                                                                              #
#                                                                              #
#            Lesotho Food Composition Table (LSFCT, 2006)                      #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

#Loading library
library(tidyverse)
# Loading functions
source("functions.R")

# Data Import ----

readxl::read_excel(here::here("FCTs", 'LS06', 'LesFCT_final_copyrevised1.xlsx'), 
                   sheet = 5)

lsfct <- readxl::read_excel(here::here("FCTs", 'LS06', 'LesFCT_final_copyrevised1.xlsx'), 
                            sheet = 6) %>%  #Reads the excel document and assigns the relevant sheet to an R data frame
  mutate(source_fct = 'LS06')  #Creates the source_fct column and fills it with "WA19_FCT"

names(lsfct)
names(lsfct) <- str_squish(names(lsfct))
names(lsfct) <- gsub(" |-", "", names(lsfct))
names(lsfct)[c(2:20, 31:40)] <- c("fdc_id", "food_desc", "Edible_factor_in_FCT", "ENERCkJ", 
                           "WATERg", "CHOAVLDFg", 'NTg', 'PROCNTg', 'PROPLAg', 'PROANIg' , 
                           'FIBTGg', 'ASHg', 'FATg',
                           'CHOLEg', 'FASATg', 'FAMSg', 'FAPUg', 'STARCHg', 'SUGARg', 
                           'VITAmg',  'CARBEQmcg', 'VITDmcg', 'VITEmg', 
                            'THIAmg', 'RIBFmg', 'NIAmg', 'VITB6_mg'  ,'FOLmg', 'VITCmg')


# Creating variable food groups

# Extracting food groups from the row in food_desc

fglso <- lsfct %>% filter(is.na(fdc_id), !is.na(food_desc)) %>% pull(food_desc) %>%
  stringr::str_split_fixed( ' ', n = 2) %>% as_tibble()

lsfct$food_group <- NA

# Assigning each food item its food group (based on their food id)
for(i in 1:nrow(fglso)){
  
  lsfct$food_group <- ifelse(grepl(fglso[i,1], lsfct$fdc_id), fglso[i,2], lsfct$food_group)
  
}

# Removing empty rows

lsfct <- lsfct %>% filter(!is.na(fdc_id))


# Standardising variables

names(lsfct)
# Replacing trace values to zero
lsfct[, c(4:40)] <- apply(lsfct[, c(4:40)] , 2, TraceToZero)
# Removing * values
lsfct[, c(4:40)]  <- apply(lsfct[, c(4:40)], 2, RemoveStar)
# Removing brackets ([])
lsfct[, c(4:40)] <- apply(lsfct[, c(4:40)] , 2, no_brackets)
# Removing empty spaces
lsfct[, c(4:40)] <- apply(lsfct[, c(4:40)] , 2, str_squish)

# Making food component variable numeric (check)
lsfct[, c(4:40)] <- apply(lsfct[, c(4:40)], 2, as.numeric)

# Unit conversion Cholesterol (g to mg)
lsfct$CHOLEmg <- lsfct$CHOLEg/1000

# Energy in kcal calculation (add function)
lsfct$ENRCkcal <- 
  
# SOP calculation (check function)
lsfct <- SOP_std_creator(lsfct)

# Adding the reference (source of nutrient) to the main LSOFCT

ref.lso <- readxl::read_excel(here::here('data','2006_LSOFCT.xlsx'), 
                              sheet = 5) %>% 
   select(c(1, 74:77)) %>% 
  filter(!is.na(code)) %>%
  rename(fdc_id = "code", nutrient_data_source = "FCT SOURCE") 

lsfct <- lsfct %>% left_join(., ref.lso %>% select(fdc_id, nutrient_data_source)) 

# Data Output ----

write.csv(Output_table, file = here::here("FCTs", "LS06_FCT_FAO_Tags.csv"), row.names = FALSE) #Saves the newly-cleaned table to the Output folder 

#Run this to clean the environment
rm(list = ls())

