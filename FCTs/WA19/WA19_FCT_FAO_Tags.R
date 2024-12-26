
################################################################################
#                                                                              #
#                           FAO/INFOODS                                        #
#        Western Africa Food Composition Table (WAFCT, 2019)                   #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Library loading 

library(tidyverse)

# 0) Accessing the data (for source of the data see README) - Uncomment!
# Only need to do it the first time to obtain the raw files!
# 
# f <- "https://www.fao.org/fileadmin/user_upload/faoweb/2020/WAFCT_2019.xlsx"
# 
# download.file(f, 
#             destfile = here::here("FCTs", 'WA19', "WAFCT_2019.xlsx"),
#             method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#             mode="wb")
# 

# Data Import ----

wafct <- readxl::read_excel(here::here("FCTs", 'WA19', 'WAFCT_2019.xlsx'), sheet = 5) %>%  #Reads the excel document and assigns the relevant sheet to an R data frame
  mutate(source_fct = 'WA19')  #Creates the source_fct column and fills it with "WA19_FCT"



# Automatic renaming ----

for( i in 8:62){ #Loops through each column between column 8 and 64
  first_row <- toString(wafct[1, i]) #Takes the first row for that column and assigns it to a variable
  second_row <- toString(wafct[2, i]) #Takes the second row for that column and assigns it to a variable
  split_string <- str_split(first_row, "\\(") #Splits the first row around "(", assigning the two resulting strings to a variable
  units_int <- gsub("\\*|\\(|\\)", "", split_string[[1]][length(split_string[[1]])]) #Separates the units out from the split_string (everything after the last open bracket)
  colnames(wafct)[i] <- paste0(second_row, units_int) #The column name is replaced with row 1 and the units from row 2
}



# Manual renaming ----

wafct_rename_names <- c("fdc_id", "food_desc", "food_descFR", "scientific_name", "nutrient_data_source",
                        "Edible_factor_in_FCT", "Edible_factor_in_FCT_2", "PROCNTg", "XFA", "XN") #Creates a list of names to apply to certain columns

wafct <- wafct %>% rename_at(vars(1:7, 12, 52, 63),  ~wafct_rename_names) #Applies the list of names to the columns at certain positions



# Creating food_groups variable and tidying ----

fgwa <- wafct %>% filter(is.na(food_desc), !is.na(fdc_id)) %>% pull(fdc_id) %>%
  stringr::str_split_fixed( '/', n = 2) %>% as_tibble() %>% pull(V1) #Creates a list of the food groups using their unique row structure in the table to identify them


wafct <- wafct %>% #Identifies the food group number from the fdc_id, and applies the correct food_group from the fgwa list to the food_group column
  mutate(food_group =
           ifelse(grepl("01_", fdc_id), fgwa[1],
                  ifelse(grepl("02_", fdc_id), fgwa[2],
                         ifelse(grepl("03_", fdc_id), fgwa[3], 
                                ifelse(grepl("04_", fdc_id), fgwa[4], 
                                       ifelse(grepl("05_", fdc_id), fgwa[5],
                                              ifelse(grepl("06_", fdc_id), fgwa[6],
                                                     ifelse(grepl("07_", fdc_id), fgwa[7],
                                                            ifelse(grepl("08_", fdc_id), fgwa[8],
                                                                   ifelse(grepl("09_", fdc_id), fgwa[9], 
                                                                          ifelse(grepl("10_", fdc_id), fgwa[10], 
                                                                                 ifelse(grepl("11_", fdc_id), fgwa[11],
                                                                                        ifelse(grepl("12_", fdc_id), fgwa[12],     
                                                                                               ifelse(grepl("13_", fdc_id), fgwa[13],
                                                                                                      ifelse(grepl("14_", fdc_id), fgwa[14],
                                                                                                             'NA'))))))))))))))) %>% 
  filter(!is.na(food_desc)) #Removes any rows without a food description entry (the food group name rows, and a row that have already been used for naming)

wafct <- wafct %>% slice(-1) #Removes the first row (which was used for the automatic renaming, and is now not useful)



# Creating low-quality dataset ----

#Creating a dataset w/ the values that were of low quality [] trace or normal
wa_nut <- wafct %>% select(Edible_factor_in_FCT:XN) %>% colnames() #selecting nutrient variable names where we want to check for quality/trace

#dataset w/ metadata info that will be removed from the dataset for use
wa_meta_quality <- wafct %>% mutate_at(wa_nut,  ~case_when(
  str_detect(. , '\\[.*?\\]') ~ "low_quality", #Looking for things in square brackets to mark as low quality
  str_detect(. , 'tr') ~ "trace", #Looking for things marked as "tr" and labels them as trace
  TRUE ~ "normal_value")) #Else it marks the entry as a normal value



# Calculating with/tidying from low quality values ----

#Extracting variables calculated with different (lower quality) method 
#and reported as using [] and removing them from the original variable

wafct <- wafct %>% 
  mutate(FATCEg = str_extract(FATg, '(?<=\\[).*?(?=\\])'),  #Creating calculated values from the lower quality method and removing the original values from the original variable
         FIBCg =  str_extract(FIBTGg, '(?<=\\[).*?(?=\\])'), #e.g. this creates the FIBCg value from the FIBTGg value
         CARTBmcg = ifelse(is.na(CARTBmcg), str_extract(CARTBEQmcg, '(?<=\\[).*?(?=\\])'), CARTBmcg), 
         TOCPHAmg = ifelse(is.na(TOCPHAmg),str_extract(VITEmg, '(?<=\\[).*?(?=\\])'), TOCPHAmg ),
         NIAmg = ifelse(is.na(NIAmg), str_extract(NIAEQmg, '(?<=\\[).*?(?=\\])'), NIAmg), 
         FOLSUMmcg = str_extract(FOLmcg, '(?<=\\[).*?(?=\\])'), 
         PHYTCPPD_PHYTCPPImg = str_extract(PHYTCPPmg, '(?<=\\[).*?(?=\\])'))



#The following f(x) removes [] and changing tr w/ 0

no_brackets_tr <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    TRUE ~ i)
}

wafct <- wafct %>% 
  mutate_at(wa_nut, no_brackets_tr) #This applies the above function



# Reordering variables ----

wafct <- wafct %>% dplyr::relocate(food_group, .after = nutrient_data_source) %>% #This moves food_group to after nutrient_data_source
  dplyr::relocate(source_fct, .before = fdc_id) #This moves source_fct before fdc_id



# Converting to numeric ----

wafct <- wafct %>% mutate_at(vars(`Edible_factor_in_FCT`:`PHYTCPPD_PHYTCPPImg`), as.numeric) #Converts certain columns (the data columns) to numeric

#Optional - check the data before saving
glimpse(wafct)

# Data Output ----

write.csv(wafct, file = here::here("FCTs", "WA19_FCT_FAO_Tags.csv"), 
          row.names = FALSE) #Saves the newly-created data table to the Output folder

#Run this to clean the environment
rm(list = ls())