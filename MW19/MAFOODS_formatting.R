

## -- ONLY RUN THE FIRST TIME ---####

#This script load the pdf and convert it into a csv file
#In addition correct some incorrect values

#install.packages("tabulizer")
#install.packages("shiny")
#install.packages("miniUI")

library(tabulizer)
library(shiny)
library(miniUI)
library(tidyverse)


##1) IMPORT MALAWI FCT FROM FAO/INFOODS 



#PDF loaded into R 

t <-  "https://dl.tufts.edu/downloads/g158bw806?filename=d217r336d.pdf"

#Identifying the area of the food compo tables in the pdf file 

#f <- locate_areas(t, pages = c(21,56))

#Extracting fct from all pages in the pdf

mwi_table <- extract_tables(t,
                            output = "data.frame",
                            pages = c(21:27, 
                                      36:37, #each one is a food group
                                      41:47, # in MAFOODS
                                      53:55, #here missing one page
                                      60:62,   #it's added below
                                      65,       #different area
                                      67:75, 
                                      78), 
                            area = list(
                              c(56, 38, 544, 800)
                            ), 
                            guess = FALSE)

mwi_table[[22]] <-  mwi_table[[22]][,-16] #removing an extra empty column 

mwi_table[34] <- extract_tables(t,                    #Getting page 56
                                output = "data.frame",
                                pages = 56,           #this is the missing page
                                area = list(          #with different area  
                                  c(73, 38, 388, 800)
                                ), 
                                guess = FALSE)

#
#mwi_table <- extract_tables(t,
#                            output = "data.frame",
#                            pages = c(21:27, #6
#                                      36:37, #1 each one is a food group
#                                      41:47, #6 in MAFOODS
#                                      53:55, #2
#                                      56,    #this is vege, but smaller table
#                                      60:62,#2
#                                      65,   #1
#                                      67:75, #8
#                                      78), #1
#                            area = list(
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(73, 38, 388, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800),
#                              c(56, 38, 544, 800)
#                            ), 
#                            guess = FALSE)
#

#Checking the structure 

mwi_table[[2]]

#from a list to a data.frame

mwi_table_clean <- reduce(mwi_table, bind_rows) %>% select(1:16)


##2) TIDYING MAFOODS INTO A LONG DATA.FRAME


#extracting variable names from fct
#second row in MAFOODS

name2 <- mwi_table[[1]] %>% slice(2) %>% as.character()

name2[1:2] <- c("food.group", "food.descr2")

#third row in MAFOODS
name4 <- mwi_table[[1]] %>% slice(5)  %>% as.character()

name4[1:2] <- c("X1", "X2")

#Cleaning steps to have information into one food item per row
#creating a long data.frame w/ 1 obs. per row


#This loop is working :)

mwi <- list()

for(i in 1:4){
  
  f <- str_which(mwi_table_clean$Code, "MW") %>% as.numeric()
  
  f <- f+i-1
  
  mwi[[i]] <- mwi_table_clean %>% slice(f)
  
  print(i)
  
}

#renaming columns and selecting those that are not empty

names1 <- mwi[[2]] %>% colnames()

mwi[[2]] <- mwi[[2]] %>% rename_at(vars(names1), ~name2) 

mwi[[3]] <- mwi[[3]] %>% rename(
  food.ref = "Code", 
  food.descr3 = "Food.Item.Name") %>% select(1:2)

mwi[[4]] <- mwi[[4]] %>% rename_at(vars(names1), ~name4) %>% select(3:16)

mwi_clean <- reduce(mwi, bind_cols) %>% janitor::clean_names()

#combining three variables w/ name of food item and reordering variables.

mwi_clean <- mwi_clean %>% unite(food_item_name, c(food_item_name,food_descr2,
                                                   food_descr3), sep = "") %>% 
  relocate(c(food_group, food_ref), .after = "food_item_name")

mwi_clean %>% head() %>% knitr::kable()

write.csv(mwi_clean, here::here("inter-output", "2019_MAFOODS.csv"), row.names = F)

# Changing the variable names to the FAO tagnames

FCT_tag <- c('code', 'fooditem', 'foodgroup', 'ref', 'WATER', 'ENERC1', 'ENERC2',
             'NT', 'PROTCNT', 'FAT', 'FASAT', 'FAMS', 'FAPU', 'CHOLE', 'CHOCSM',
             'CHOAVLDF','SUGAR', 'SUGAD', 'FIBC', 'STARCH', 'ASH', 'CA', 'FE', 
             'MG', 'P', 'K', 'NA.', 'ZN', 'CU', 'MN', 'ID', 'SE', 'VITA_RAE',
             'VITA', 'THIA', 'RIBF', 'NIA', 'VITB6', 'FOL', 
             'VITB12', 'PANTAC', 'BIOT', 'VITC', 'VITD', 'VITE' ,'PHYT')


mwi_clean <- mwi_clean %>% rename_all( ~ FCT_tag)

##4) Removing [] and ()


#This keeps numbers and create a variable with low_quality


nut <- c('WATER', 'ENERC1', 'ENERC2', 'NT',
         'PROTCNT', 'FAT', 'FASAT', 'FAMS', 'FAPU', 'CHOLE', 'CHOCSM', 'CHOAVLDF',
         'SUGAR', 'SUGAD', 'FIBC', 'STARCH', 'ASH', 'CA', 'FE', 'MG', 'P', 'K', 'NA.',
         'ZN', 'CU', 'MN', 'ID', 'SE', 'VITA_RAE', 'VITA', 'THIA', 'RIBF', 'NIA', 'VITB6', 'FOL', 
         'VITB12', 'PANTAC', 'BIOT', 'VITC', 'VITD', 'VITE' ,'PHYT')


#Creating a dataset w/ the values that were of low quality (TRUE/FALSE)
mwi_low_quality <- mwi_clean %>% mutate_at(nut, ~str_detect(., '\\[.*?\\]'))

#The following f(x) removes [] and ()

no_brackets <- function(i){
  case_when(
    str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    str_detect(i, '\\(.*?\\)') == TRUE ~ str_extract(i, '(?<=\\().*?(?=\\))'),
    TRUE ~ i)
}


#removing [] and () and converting nutrient variables (nut) into numeric

mwi_clean <- mwi_clean %>% mutate_at(nut, no_brackets) %>% 
  mutate_at(nut, as.numeric)



##5) Changing mineral values in ref.10 to reconverted values

#Filtering data entries in MAFOODS from Joy et al. paper
#Not the same as in the excel (WHY!!??)


mwi_water <- mwi_clean %>% dplyr::filter(ref == "10") %>% 
  arrange(code) %>% pull(code) 


mafoods_water <- c("MW01_0011", "MW01_0016", "MW01_0037", "MW01_0060",
                   "MW01_0065" ,"MW02_0007", "MW02_0014", "MW02_0019",
                   "MW04_0004", "MW04_0011", "MW04_0012", "MW04_0019",
                   "MW04_0020", "MW04_0031", "MW04_0036", "MW05_0016",
                   "MW05_0019")

mwi_water == mafoods_water

setdiff(mwi_water, mafoods_water)


EJ <- read.csv(here::here('data',
                          'mineral-composition_2020-11-06.csv')) %>% 
  select(-contains('median'))


#Changing mineral values on the data set (see documentation)


mwi_water <- mwi_clean %>% dplyr::filter(code %in% mafoods_water) %>% 
  inner_join(., EJ, by = c('code' = 'water_ref')) %>% 
  mutate(
    CA = ca_mg_100g, 
    CU = cu_mg_100g, 
    FE = fe_mg_100g, 
    MG = mg_mg_100g,
    SE = se_mcg_100g, 
    ZN = zn_mg_100g) %>% 
  select(1:46) %>% 
  rename(fooditem = "fooditem.x")

#Substituting old (incorrect) values to new values

mwi_clean <- mwi_clean %>% dplyr::filter(!code %in% mafoods_water) %>% 
  bind_rows(., mwi_water)

write.csv(mwi_clean, here::here("inter-output", "2019_MAFOODS_with-corrections.csv"), row.names = F)


