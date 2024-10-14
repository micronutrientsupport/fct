
# Loading library
library(dplyr)
library(NutritionTools)

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

## Loading the FC library
source(here::here("MAPS_fct_load.R"))

# Getting the most up-to-date file
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]
# Loading the file (dictionary-to-FCTs-matches)
dict_comp <- read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")



food_list <- readRDS(here::here("inter-output", 
              sort(list.files(here::here("inter-output"), 
             "food-list_ihs4"), decreasing = TRUE)[1])) %>% 
  #Removing food name as it is wrong in many cases
  select(-food_name) %>% #adding the correct FoodName_3
    left_join(., dictionary.df[, c("ID_3", "FoodName_3")])

names(food_list)

# Checking excluded in IHS5 script (ihs5_genus.R)
# Here's the changes that were made however no serious issue
# or good reason seemed recoded (beside data needs)
# Not changing them back
food_list %>% 
filter(ID_3 %in% # Citrus and orange sweet potato from boiled version
        c("1530.06", "1321.02", "1322.02", "1324.02", 
          # Changed green and roasted maize to boiled 
          "1290.01.01", "1290.01.02" , "1290.01.03",
          # Yeast and baking powders 
              "F1232.07", "F1232.06",
        ))  %>% 
  View()

food_list %>% 
  filter(ID_3 %in%  # Three traditional drinks used the same code ()
           c( "24310.02.01",
              # Wrongly coded Nkate
              "F0022.06" ,
              # Changing food consumed at restaurant
              "F1061.01","F1232.05"
              ))  %>% View()

# Checking code
food_list %>% 
  filter(code %in% c("816", "832" , "836", "106", "510") &
           ID_3 %in% c("F0666.01", "1530.07", "F0022.06",
                                "23161.01.01", "21119.01.01"))


food_list %>% 
  filter(ID_3 %in%  # Three traditional drinks used the same code ()
           c( 
             "23161.01.01" ))  

dictionary.df %>% 
  filter(ID_3 %in%  
           c("1322.01")) 

fct_dict %>% 
  filter(ID_3 %in%  
           c("F1232.06"))  %>%
  select(source_fct, ID_3,  food_desc,  CAmg, SEmcg, VITA_RAEmcg, VITB12mcg) %>% 
  View()

# good.matches %>% 
#   left_join(., fct_dict, by =c("food_genus_id" = "ID_3", 
#                                                "ref_fctcode" = "fdc_id")) %>% 
#   View()
  
  fct_dict %>% 
    filter(grepl("banana", food_desc, ignore.case = TRUE) &
    grepl("maize", food_desc, ignore.case = TRUE) &
             source_fct %in% c("MW19", "KE18", "WA19"))  %>%
    select(source_fct, ID_3,  fdc_id,food_desc,  CAmg, SEmcg, VITA_RAEmcg, VITB12mcg) %>% 
  View()

# Fixing ID_3 ----
# groundnuts (to dry and shelled) - Corrections should be done at Edible Portion
food_list$ID_3[food_list$code %in% c("311", "313")] <-  "142.01" 
food_list$ID_3[food_list$code %in% c("505")] <-  "21116.03" 
food_list$ID_3[food_list$ID_3 %in% c("21170.01.01")] <-  "21170.01.03" 
food_list$ID_3[food_list$ID_3 %in% c("1501.11")] <-  "1501.10" 
food_list$ID_3[food_list$ID_3 %in% c("1501.05")] <-  "1503.03" 
food_list$ID_3[food_list$ID_3 %in% c("21121.02")] <-  "21121.03" 
food_list$ID_3[food_list$ID_3 %in% c("21121.02")] <-  "21121.03" 
#Fixing samosa vs banana cake id typo issue (see MAPS_Dictionary-Protocol update v2.5)
food_list$ID_3[food_list$ID_3 == "F0022.06" & food_list$code == "836" ] <- "F0022.07"
#Meal eaten at restaurant (vendor) c(21116.02, 21121.04) -->  F1061.01 
#TODO: Generate a recipe c(F1061.01,F1232.05 )
food_list$ID_3[food_list$code == "829"] <- "F1061.01,F1232.05"
# Chicken at vendor (assumed cooked)
food_list$ID_3[food_list$code == "824"] <- "F1061.02,F1061.01"

## Separating one-to-many matches
food_list <- food_list %>% separate_rows(ID_3)
# Excluding item (dupli)
food_list <- food_list[!food_list$ID_3 %in% c("21184.01.02", "21119.01.01"), ]

# Checking
food_list[food_list$code %in% c("311", "313"), ] %>% View()

food_list %>% select(code, item, ID_3) %>% distinct() %>% 
  left_join(., dictionary.df) %>% select(1:3, FoodName_3, FoodName_1) %>% # View()
   filter(is.na(FoodName_3))

food_list %>% select(code, item, ID_3) %>% distinct() %>% 
  left_join(., dict_comp) %>% select(1:3, fdc_id) %>% 
  filter(is.na(fdc_id)) %>% View()

# New clean (proper names & food group (FoodName_1))
food_list <- food_list %>% select(code, item, ID_3) %>% distinct() %>% 
  left_join(., dictionary.df) %>% select(1:3, FoodName_3, FoodName_1) 

fct_dict %>% filter(grepl("pork|beef|goat|mutt|pig ", food_desc, ignore.case = TRUE) &
                      !grepl("raw|egg|milk", food_desc, ignore.case = TRUE) &
                      source_fct %in% c("MW19", "KE18")) %>% 
  select(source_fct, fdc_id, food_desc, ID_3, WATERg, SEmcg) %>% View()

# food_list %>% select(code, item, ID_3) %>% distinct() %>% 
#   left_join(., dictionary.df) %>% select(1:3, FoodName_3, FoodName_1) %>% 
#   write.csv(., here::here("inter-output", "ihs4_dictionary_foodgroup1.csv" ), 
#             row.names = FALSE)

# Getting the NCT for IHS4 -----

# This matches are tailored to Se, but it can be done for any other MN
# Nutrient of interest
mn <- "SEmcg"

# fct1 <- "Joy et al, 2015"
fct1 <- "MW19"
fct2 <- "KE18"
fct3 <- "IN"
fct3 <- "UK21"
fct4 <- "US19"

fct <- food_list %>% left_join(., fct_dict %>%
                filter(source_fct %in% fct1 & !is.na(!!sym(mn)))) %>% 
  filter(!is.na(source_fct))

fct <- food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% 
  left_join(., fct_dict %>%
                          filter(source_fct %in% fct2 & !is.na(!!sym(mn)))) %>%
   filter(!is.na(source_fct)) %>% 
  bind_rows(., fct)

fct <- food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% 
  left_join(., fct_dict %>%
              filter(source_fct %in% fct3 & !is.na(!!sym(mn)))) %>%
  filter(!is.na(source_fct)) %>% 
  bind_rows(., fct)

fct <- food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% 
  left_join(., fct_dict %>%
              filter(source_fct %in% fct4 & !is.na(!!sym(mn)))) %>%
  filter(!is.na(source_fct)) %>% 
  bind_rows(., fct)

food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% 
  left_join(., fct_dict %>%
              filter(#source_fct %in% fct4 &
                !is.na(!!sym(mn)))) %>%
  filter(!is.na(source_fct)) %>% 
  select(source_fct, fdc_id, food_desc, ID_3, WATERg, SEmcg)

#fct <- food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% 
#  left_join(., fct_dict %>%
#              filter(source_fct == "UK21" & !is.na(SEmcg))) %>%
#  filter(!is.na(source_fct)) %>% 
#  bind_rows(., fct)

## Provisional matches for IHS4

# fct %>% 
#   select(code, item, source_fct, ID_3,  fdc_id,food_desc, WATERg, ENERCkcal, 
#          SEmcg) %>% 
#   write.csv(here::here("inter-output", "nct", 
#                        "ihs4-partial-mw19-ke18-uk21-us19_nct_v1.0.0.csv"),
#             row.names = FALSE)
 
# Checking if still missing values
food_list %>% filter(!ID_3 %in% unique(fct$ID_3)) %>% View()
#  left_join(., dict_comp %>%
#              filter(source_fct == "KE18")) %>%
#  filter(is.na(source_fct)) %>% View()

# Checking variables names  
names(fct)
sum(duplicated(fct$code)) # If one-to-many matches

# Combining using the Group summariser for a better looking export
#fct$code <- as.character(fct$code)
#test <- Group_Summariser(fct, "code")

# Combining one-to-many matches
fct <- fct %>% group_by(code, item) %>% 
  summarise( 
   source_fct = paste0(source_fct, collapse = "; "), 
    fdc_id = paste0(fdc_id, collapse = "; "),
     ID_3 = paste0(ID_3, collapse = "; "), 
   across(c("ENERCkcal", "WATERg", mn), as.numeric),
   across(is.numeric, median)) 
  
# Checking columns of interest
sum(fct[, is.na(mn)])
sum(is.na(fct$ENERCkcal))

# Getting the loaded data 
  
## NCT data (IHS5) (from fct repo)
nct <-   read.csv(here::here("output", "fct_ihs5_v2.2.csv")) %>%   
    # Selecting only variables of interest
    select(1:5, WATER, ENERC1, SE, comment) %>% rename(
      code = "ihs5_foodid", 
      item = "ihs5_fooditem", 
      source_fct = "ref_source", 
      fdc_id = "ref_fctcode", 
      #   food_desc = "ref_fctitem", 
      WATERg = "WATER", 
      ENERCkcal = "ENERC1", 
      SEmcg = "SE", 
      comments = "comment"
    ) %>% mutate(ID_3 = NA)
  
  
head(nct)
sum(duplicated(nct$code))

# Combining dataset to complete IHS4 NCT (for Se)
fct$code <- as.character(fct$code)
nct %>% filter(!code %in% unique(fct$code)) %>% 
  bind_rows(., fct) %>% 
  # Only missing values for baby milk (not consumed by WRA & mucuna not reported in IHS4)
  filter(is.na(!!sym(mn))) 

nct <- nct %>% filter(!code %in% unique(fct$code)) %>% 
  bind_rows(., fct) %>% 
  # Only missing values for baby milk (not consumed by WRA & mucuna not reported in IHS4)
  filter(!is.na(!!sym(mn)))

# Saving the NCT  ---------
# Nutrient of interest
# nct %>% 
#     write.csv(., here::here("inter-output", "nct", 
#                   paste0("ihs4_nct_", paste(mn, sep ="-"), "_v1.0.0.csv")),
#               row.names = FALSE)


names(nct)
head(nct)

# Food items - NCT has 3 extra (b) for biofortification scenario
length(unique(nct$code)) == length(unique(food_list$code))

# Proposed matches not complete
nrow(nct)-2 == nrow(food_list)

# Checking what is misssing 
food_list$code <- as.character(food_list$code)
food_list %>% left_join(., nct) %>% 
  filter(is.na(fdc_id)) %>% View()

fct %>% left_join(., fct_dict, by =c("ID_3", "fdc_id", "source_fct")) %>% 
  select(code, item, ID_3, fdc_id, source_fct, WATERg, ENERCkcal, SEmcg) %>% 
  filter(is.na(SEmcg)) %>% View()
