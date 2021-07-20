

#install.packages("tabulizer")
#install.packages("shiny")
#install.packages("miniUI")

library(tabulizer)
library(shiny)
library(miniUI)
library(tidyverse)


#PDF loaded into R -

t <-  "https://dl.tufts.edu/downloads/g158bw806?filename=d217r336d.pdf"

f <- locate_areas(t, pages = c(21,56))

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

mwi_table[34] <- extract_tables(t,
                            output = "data.frame",
                            pages = 56,           #this is the missing page
                            area = list(          #with different area  
                              c(73, 38, 388, 800)
                            ), 
                            guess = FALSE)


mwi_table <- extract_tables(t,
                            output = "data.frame",
                            pages = c(21:27, #6
                                      36:37, #1 each one is a food group
                                      41:47, #6 in MAFOODS
                                      53:55, #2
                                      56,    #this is vege, but smaller table
                                      60:62,#2
                                      65,   #1
                                      67:75, #8
                                      78), #1
                            area = list(
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(73, 38, 388, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800),
                              c(56, 38, 544, 800)
                            ), 
                            guess = FALSE)

#from a list to a data.frame

mwi_table_clean <- reduce(mwi_table, bind_rows)

#mwi_table_clean <- mwi_table_clean %>% select(1:16)

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

##3) Changing the variable names to the FAO tagnames

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

mwi_clean %>% dplyr::filter(ref == "10")


EJ <- read.csv(here::here('data',
                          'mineral-composition_2020-11-06.csv')) %>% 
  select(-contains('median'))


##6) MAPS type format

#Adding GENuS code and confidence

#Rename variables according to MAPS-standards




###========================= END =============================###

mwi_table <- extract_tables(t,
                            output = "data.frame",
                            pages = 21)

mwi_table <- extract_tables(t,
method = "decide", 
output = "data.frame")


mwi_table %>% 
pluck(13) %>% 
  as_tibble() %>% head() %>% knitr::kable()



mwi_raw_tbl <- mwi_table %>% 
  pluck(12) %>% 
  as_tibble()

# Show first 6 rows
mwi_table_clean %>% head() %>% knitr::kable()

# Get column names from Row 1
col_names <- mwi_table_clean %>% 
  slice(1) %>%  pivot_longer(cols = everything()) %>%
  mutate(value = ifelse(is.na(value), "Missing", value)) %>%
  pull(value)

mwi_table_clean_rename <- mwi_table_clean %>%
  set_names(col_names) %>%
  slice(-1)

# Show first 6 rows
mwi_table_clean_rename %>% head() %>% knitr::kable()


#    mwi_table_clean %>% filter(str_detect(X, "MW")) 
#    
#    
#    f <- str_which(mwi_table_clean$X, "MW")
#    
#    mwi <- as.data.frame(1:69)
#    
#    for(i in 1:2){
#    
#      f <- f+1
#    
#     mwi[[i]] <- mwi_table_clean %>% slice(f)
#    
#    }
#


#This loop is working :)

mwi <- list()

for(i in 1:4){
  
  f <- str_which(mwi_table_clean$X, "MW") %>% as.numeric()
  
  f <- f+i-1
  
  mwi[[i]] <- mwi_table_clean %>% slice(f)
  
  print(i)
  
}


mwi[[4]] %>% filter(str_detect(X, id))

mwi_clean <- reduce(mwi, bind_cols)





mwi_clean %>% relocate(starts_with("GROUP.1"), .after = "X...1") %>% 
  mutate(water = str_extract(GROUP.1..STAPLES...2, "[:digit:]{1,2}\\.[:digit:]{1,2}"),
         water.low = str_detect(GROUP.1..STAPLES...2, "\\[*\\]"))

compo <- "[:digit:]{1,3}\\.[:digit:]{1,2}|[:digit:]{1,4}"

micro <- "[:digit:]{1,3}\\.[:digit:]{1,2}"

ener <- "[:digit:]{1,4}"

bracket <- "\\[.*?\\]"

all <- "[:digit:]{1,3}\\.[:digit:]{1,2}|\\[.*?\\]"

id <- "MW[:digit:]{2}\\_[:digit:]{4}"

food.group <- "[:upper:]{1}[:lower:]{2,}"

ref <- "R?[:digit:]{1,2}|[:upper:]{2,4}"

mwi_clean <- mwi_clean %>% relocate(starts_with("GROUP.1"), .after = "X...1") %>% 
  mutate(moisture_in_g = str_extract(GROUP.1..STAPLES...2, compo),
         water.low = str_detect(GROUP.1..STAPLES...2, bracket), 
         food.description = str_replace(GROUP.1..STAPLES...2, 
                                  all, "")) %>% 
  mutate(fibre_in_g = str_extract(GROUP.1..STAPLES...16, compo),
         fibre.low = str_detect(GROUP.1..STAPLES...16, bracket), 
         food.description2 = str_replace_all(GROUP.1..STAPLES...16, all, ""), 
         fibre.low = ifelse(is.na(fibre_in_g), str_detect(food.description, bracket), fibre.low),
         fibre_in_g = ifelse(is.na(fibre_in_g), str_extract(food.description, compo), fibre_in_g), 
         food.description = str_replace(food.description, 
                                        all, "")) %>% 
  mutate(vitamina_in_rae_in_mcg = ifelse(str_detect(GROUP.1..STAPLES...30, compo), 
                                         GROUP.1..STAPLES...30, str_extract(GROUP.1..STAPLES...44, compo)))

  
mwi_clean <- mwi_clean %>% relocate(starts_with("X..."), before = "GROUP.1..STAPLES...2") %>% 
  mutate(code = str_extract(X...1, id), 
         food.group = ifelse(str_detect(X...15, food.group), 
                                        X...15, str_extract(X...1, food.group)),
         ref = ifelse(str_detect(X...29, ref),X...29, 
                      str_extract(X...15, ref)), 
         food.name = str_c(food.description, food.description2, sep = "")) %>% 
  relocate(c("food.name", "water"), .after = "ref")


mwi_clean %>% relocate(starts_with("X.1."), .after = "X...1") %>% 
  separate(Per.100g.edible.food...8, c("SFA", "MSFA"), sep = " ") %>% 
  mutate(energy_in_kcal = X.1...3, 
         energy_in_kj = X.2...4,
         nitrogen_in_g = X.3...5, 
         totalprotein_in_g = X.4...6, 
         totalfats_in_g = X.5...7,
         saturatedfa_in_g = str_extract(SFA, micro),
         monounsaturatedfa_in_g	 = str_extract(MSFA, micro),
         polyunsaturatedfa_in_g = X.6...9,
         cholesterol_in_mg = X.7...10,
         carbohydrates_total_in_g = X.8...11,
         carbohydrates_avail_in_g = X.9...12, 
         )
