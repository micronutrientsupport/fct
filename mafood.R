

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

###############################################################################
##This keeps numbers and create a variable with low_quality -NOT WORKING-
#for (i in nut) {    
#   new_col_name <- paste0("low_", i)
#   
#   clean_data <- mwi_clean %>% 
#     mutate(!!sym(new_col_name) := str_detect(i, '\\[.*?\\]'))
#   
#   print(i)
# 
#   }                       
############################################################################## 

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


##3) MAPS type format

#Loading data

source("dictionary.R")

dictionary.df %>% filter(ID_3 == "01520.01.03")

mwi_clean %>% filter(code == "MW01_0041")

#Adding GENuS code

mwi_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
"MW01_0010" ,  "01520.01.02", "h",
"MW01_0011" ,  "01520.01.01", "h",
"MW01_0013" ,  "1550.01", "h",
"MW01_0014" ,  "F0022.04", "h",
"MW01_0016" ,  "118.02", "h",
"MW01_0017" ,  "23170.01", "h",
"MW01_0018" ,  "23120.03.02", "h",
"MW01_0019" ,  "23120.03.01", "h",
"MW01_0025" ,  "1510.02", "h",
"MW01_0037" ,  "112.01", "h",
"MW01_0040" ,  "1290.01.03", "h",
"MW01_0041" ,  "F0022.02", "h",
"MW01_0048" ,  "1313.01", "h",
"MW01_0050" ,  "1510.01", "h",
"MW01_0058" ,  "23161.01.01", "m",
"MW01_0060" ,  "114.01", "h",
"MW01_0063" ,  "1530.02", "h",
"MW01_0065" ,  "1530.01", "h",
"MW01_0066" ,  "1530.04", "h",
"MW02_0004" ,  "1701.03", "h",
"MW02_0007" ,  "1706.02", "h",
"MW02_0010" ,  "142.02", "h",
"MW02_0012" ,  "141.02", "h",
"MW02_0014" ,  "142.01", "h",
"MW02_0015" ,  "142.05", "h",
"MW02_0017" ,  "1707.01", "h",
"MW02_0019" ,  "141.01", "h",
"MW03_0006" ,  "21111.01.01", "h",
"MW03_0010" ,  "21121.03", "h",
"MW03_0011" ,  "21121.01", "h",
"MW03_0013" ,  "231.02", "h",
"MW03_0015" ,  "231.01", "h",
"MW03_0019" ,  "1533.01", "h",
"MW03_0030" ,  "1501.03", "h",
"MW03_0052" ,  "21116.01", "h",
"MW03_0059" ,  "2211.01", "h",
"MW03_0063" ,  "21115.01", "h",
"MW03_0064" ,  "21113.02.01", "h",
"MW03_0065" ,  "21170.01.03", "h",
"MW03_0067" ,  "21114.01", "h",
"MW04_0003" ,  "1212.02", "h",
"MW04_0004" ,  "1212.01", "h",
"MW04_0019" ,  "1214.04", "h",
"MW04_0020" ,  "1214.03", "h",
"MW04_0025" ,  "1270.01", "h",
"MW04_0030" ,  "1239.01.01", "h",
"MW04_0031" ,  "1253.02.01", "h",
"MW04_0034" ,  "1235.01", "h",
"MW04_0036" ,  "1234.01", "h",
"MW05_0001" ,  "1341.01", "h",
"MW05_0002" ,  "1311.01", "h",
"MW05_0004" ,  "1312.01", "h",
"MW05_0008" ,  "1316.02", "h",
"MW05_0016" ,  "1316.01", "h",
"MW05_0019" ,  "1317.01", "h",
"MW05_0021" ,  "1318.01", "h",
"MW06_0001" ,  "21700.02.01", "h",
"MW08_0007" ,  "1802.01", "h",
"MW08_0008" ,  "2899.01.01", "h")

mwi_genus <-  mwi_genus %>% left_join(., dictionary.df)

#Adding genus variables and 
#Rename variables according to MAPS-standards

MAPS_mwi <- mwi_clean %>% 
left_join(., mwi_genus, by = c("code" = "ref_fctcode")) %>% 
  mutate(fct_name = "MAFOODS", 
         folicacid_in_mcg = "NA") %>% 
  rename(
  original_food_id = "code",
  original_food_name = "fooditem",
  food_genus_id = "ID_3",
  food_genus_description = "FoodName_3",
  food_group = "FoodName_0",
  food_subgroup = "FoodName_1",
  food_genus_confidence = "confidence",
  data_reference_original_id = "ref",
  moisture_in_g = "WATER",
  energy_in_kcal = "ENERC1",
  energy_in_kj = "ENERC2",
  nitrogen_in_g = "NT",
  totalprotein_in_g = "PROTCNT",
  totalfats_in_g = "FAT",
  saturatedfa_in_g = "FASAT", 
  monounsaturatedfa_in_g = "FAMS", 
  polyunsaturatedfa_in_g = "FAPU", 
  cholesterol_in_mg = "CHOLE",
  carbohydrates_in_g = "CHOAVLDF", 
  fibre_in_g = "FIBC", 
  ash_in_g = "ASH",
  ca_in_mg = "CA", 
  fe_in_mg = "FE",
  mg_in_mg = "MG",
  p_in_mg = "P",
  k_in_mg = "K",
  na_in_mg = "NA.", 
  zn_in_mg = "ZN", 
  cu_in_mg = "CU",
  mn_in_mcg = "MN",
  i_in_mcg = "ID",
  se_in_mcg = "SE",
  vitamina_in_rae_in_mcg = "VITA_RAE", 
  thiamin_in_mg = "THIA",
  riboflavin_in_mg = "RIBF", 
  niacin_in_mg = "NIA", 
  vitaminb6_in_mg = "VITB6", 
  folate_in_mcg = "FOL",
  vitaminb12_in_mcg = "VITB12",
  pantothenate_in_mg = "PANTAC",
  biotin_in_mcg = "BIOT",
  vitaminc_in_mg = "VITC",
  vitamind_in_mcg = "VITD",
  vitamine_in_mg = "VITE", 
  phyticacid_in_mg = "PHYT") %>% 
  select(var.name)

#MAPS_mwi %>% readr::write_excel_csv(., 
 #             here::here('output', 'MAPS_MAFOODS_v1.5.csv'))#that f(x) is to 


###========================= END =============================###