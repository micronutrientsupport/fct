library(tidyverse)



# Data Import ----

UF16_Raw <- readxl::read_excel(here::here('UF16', "uFiSh1.0.xlsx"), sheet = 4) #Imports sheet 4 from the excel document



# Column Renaming (General) ----

for(i in 1:ncol(UF16_Raw)){ #This for loop cleans brackets and spaces from the column names
  original_colname <- colnames(UF16_Raw)[i] #creates a variable for the column name in question
  if(grepl("(", original_colname, fixed = TRUE)){ #Checks to see if theres an open bracket in the column name
    #print(paste0(original_colname, " contains ("))
    cleaned_name <- gsub(" \\(|\\(|\\)", "", original_colname) #If so, then brackets and spaces are removed
    #print(cleaned_name)
    colnames(UF16_Raw)[i] <- cleaned_name #And the newly cleaned name replaces the old column name
  }
}



# Column Renaming (Manual) and tidying ----

UF16_Raw$source_fct <- "UF16" #Creates and populates the source_fct column

Output_table <- UF16_Raw %>%
  slice(-c(1)) %>% #removes the first row
  relocate(source_fct, .after = RefID) #moves the source_fct column to the beginning with other metadata columns

special_names <- c('fdc_id', "3_alpha_code", 'food_desc', "cook_state", 
                   "nutrient_data_source", "Edible_factor_in_FCT", "EDIBLE2",
                   "PROCNTg") # Creates a list of new names as per FAO dataset

no_brackets_tr <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]|Tr') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    TRUE ~ i)
}

Output_table <- Output_table %>% 
  rename_at(vars(1, 3:4, 6:7, 9:10, 15),  ~special_names) %>% #Applies the new names in the correct places, replacing the old column names
  mutate_at(vars(9:ncol(Output_table)), no_brackets_tr)

#Optional - check the data before saving
glimpse(Output_table)

# Data Output ----

write.csv(Output_table, file = here::here("Output",  "UF16_FCT_FAO_Tags.csv"),
          row.names = FALSE) #Saves the newly-created data table to the Output folder 

#Run this to clean the environment
rm(list = ls())