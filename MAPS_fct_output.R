
# Script that format FCTs into MAPS standard name and structure
rm(list = ls())

# Load libraries
library(tidyverse)
library(viridis)
#install.packages("devtools")
#devtools::install_github("TomCodd/NutritionTools")
library(NutritionTools)
source("functions.R")

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()

# Adjusting variable names to updated version (V.1.6 updated on 2021/10/13)
var.name <- gsub("phyticacid", "phytate", var.name)

# Generating and updating the FCT-dict matches for each FCT - Uncomment!
# Only need to be run the first time, and when pulling a new version/ update
# Indiv. FCT matches can be found in the FCT_dict.R)

#fcts <- list.files(here::here("FCTs") , "*_dict.R")
#eval(parse(text = paste0("source('FCTs/", fcts, "')")))

# Getting the most up-to-date file
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]
# Loading the file (dictionary-to-FCTs-matches)
dict_comp <- read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")

dict_comp %>% count(source_fct) 

#1) Loading all FCDBs into one single database ----

#finding all the cleaned FCTs/FCDBs from the output folder
# list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, #so it is not taking the fcts in the folder
#            full.names=TRUE) %>% 
#   map_df(~read_csv(., col_types = cols(.default = "c"), 
#                    locale = locale(encoding = "Latin1")))  

# Loading all the cleaned FCTs/FCDBs into one single object (data.frame)
fct_cover <- list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))) 

# checking that we have loaded all the FCT/FCDBs (n=5)
fct_cover %>% count(source_fct) 
colnames(fct_cover)

fct_cover$fdc_id[fct_cover$source_fct %in% c("JA15", "LS06")] <- gsub("^0", "", fct_cover$fdc_id[fct_cover$source_fct %in% c("JA15", "LS06")])
fct_cover$fdc_id <- str_squish(fct_cover$fdc_id )
fct_cover$source_fct <- str_squish(fct_cover$source_fct )

# Merging dict codes to fcts codes

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3)) %>% count(source_fct)

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3) & source_fct == "WA19") %>% pull(fdc_id)

fct_dict <- fct_cover %>% left_join(., dict_comp)

dictionary.df %>% 
  filter(str_detect(ID_3, "\\b")) %>% 
  left_join(., fct_dict , by = "ID_3") %>% 
  filter(is.na(source_fct)) %>% select(FoodName_3, ID_3)

# Checking items ----
desc1 <- "maize"
desc2 <- "grain"
desc3 <-  "green"

subset(dictionary.df, grepl(desc1, FoodName_3, ignore.case = TRUE) &
         grepl(desc2, FoodName_3, ignore.case = TRUE))

subset(fct_dict, grepl(desc1, food_desc, ignore.case = TRUE) &
         grepl(desc2, food_desc, ignore.case = TRUE))

subset(dictionary.df, grepl("23914", ID_3))
subset(fct_dict, grepl("23914", ID_3)) %>% View()

sort(names(fct_dict))

# Check & remove bc it's causing issues when running the code
# fct_dict %>% filter(!is.na(PHYTCPPDmg)) %>% distinct(source_fct)

# Checking for duplicates

fct_dict %>% filter(!is.na(ID_3)) %>% 
  select(fdc_id, source_fct) %>% duplicated() %>% which()

fct_dict %>% filter(!is.na(ID_3)) %>% 
  select(fdc_id, source_fct) %>% .[798,]

 fct_dict %>% filter(fdc_id == "04_011")

## Standardisation: 

# ToDO: Generate variable when not present in the original FCT
# but needed for MAPS tool - data format standards. 

# Double-check as this bit, it is creating a problem when running
# When the FCTs if variables are not present it will cause a break. 

fct_dict <- fct_dict %>% 
  nia_conversion_creator()  %>%  # Standardisation of NIAmg
 THIAmg_std_creator() %>%  # Standardisation of THIAmg
  VITB6mg_std_creator()  # Standardisation of VITB6mg

#Saving for other works
# fct_dict %>% #filter(!is.na(ID_3)) %>% 
#   write.csv(., here::here("inter-output", "FCTs_dict_compiled_v1.0.3.csv"),
#             row.names = FALSE)


 ## MAPS Standardisation: 

## Quality Adjustments (See MAPS_fct_QA.qmd for details)
# Millet (Fe max. value 67mg/kg) 
id2 <- "^118"
max_Value <- 67/10*(100-8.67)/100

fct_dict <- fct_dict %>% 
  mutate(comment = ifelse(grepl(id2, ID_3, ignore.case = TRUE) & as.numeric(FEmg)> max_Value, "Fe adjusted", NA), 
         FEmg = ifelse(grepl(id2, ID_3, ignore.case = TRUE) & as.numeric(FEmg)> max_Value, 67/10*(100-as.numeric(WATERg))/100, FEmg)) 

fct_dict  %>% 
  filter(!is.na(comment)) %>% select(food_desc, FEmg, comment)
  
# Excluding Se from US19
fct_dict$SEmcg[fct_dict$source_fct == "US19"] <- NA

# Reducing Vitamin A in Red palm oil (reduced 50%)
food <- c("9010", "11_004")
fct <- c("KE18", "WA19")
text1 <- "VITA_RAEmcg & VITAmcg were adjusted down 50%"

#VITA_RAEmcg
value <- as.numeric(fct_dict$VITA_RAEmcg[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food])*0.5
fct_dict$VITA_RAEmcg[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food] <- value
#VITAmcg
value <- as.numeric(fct_dict$VITAmcg[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food])*0.5
fct_dict$VITAmcg[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food] <- value

comme <- fct_dict$comments[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food]

if(!sum(is.na(comme))==0){
fct_dict$comments[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food] <- text1

}else{ 
  fct_dict$comments[fct_dict$source_fct %in% fct & fct_dict$fdc_id %in% food] <- gsub("NA", "", paste0(comme, ";", text1))
}

# Assuming zero for b12 in fermented cassava products
food <- c("02_041", "02_038", "02_040", "02_039")
text1 <- "VITB12mcg assumed zero"

fct_dict$VITB12mcg[fct_dict$fdc_id %in% food] <- 0

comme <- fct_dict$comments[fct_dict$fdc_id %in% food]

if(!sum(is.na(comme))==0){
  fct_dict$comments[fct_dict$fdc_id %in% food] <- text1
  
}else{ 
  fct_dict$comments[fct_dict$fdc_id %in% food] <- gsub("NA", "", paste0(comme, ";", text1))
}


# Assuming zero for Ca in tea leaves
food <- ("537")
text1 <- "CAmg assumed zero"

fct_dict$CAmg[fct_dict$fdc_id %in% food] <- 0

comme <- fct_dict$comments[fct_dict$fdc_id %in% food]

if(!sum(is.na(comme))==0){
  fct_dict$comments[fct_dict$fdc_id %in% food] <- text1
  
}else{ 
  fct_dict$comments[fct_dict$fdc_id %in% food] <- gsub("NA", "", paste0(comme, ";", text1))
}


# Rename variables according to MAPS-standards
  MAPS_output  <- fct_dict %>% 
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>%   
  rename(
    original_food_id = "fdc_id",
    original_food_name = "food_desc",
    original_food_group = "food_group",
    food_genus_id = "ID_3",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1", 
    food_genus_confidence = "confidence",
    fct_name = "source_fct",
    data_reference_original_id = "nutrient_data_source",
    moisture_in_g = "WATERg",
    energy_in_kcal = "ENERCkcal",
    energy_in_kj = "ENERCkJ",
    totalprotein_in_g = "PROCNTg",
    totalfats_in_g = "FATg",
    saturatedfa_in_g = "FASATg", 
    monounsaturatedfa_in_g = "FAMSg", 
    polyunsaturatedfa_in_g = "FAPUg", 
    cholesterol_in_mg = "CHOLEmg",
    carbohydrates_in_g = "CHOAVLDFg", 
    fibre_in_g = "FIBTGg", 
    ash_in_g = "ASHg",
    ca_in_mg = "CAmg", 
    fe_in_mg = "FEmg",
    mg_in_mg = "MGmg",
    p_in_mg = "Pmg",
    k_in_mg = "Kmg",
    na_in_mg = "NAmg", 
    zn_in_mg = "ZNmg",
    se_in_mcg = "SEmcg",
    vitamina_in_rae_in_mcg = "VITA_RAEmcg", 
    thiamin_in_mg = "THIAmg_std",
    riboflavin_in_mg = "RIBFmg", 
    niacin_in_mg = "nia_conversion_std", 
    folate_in_mcg = "FOLFDmcg",
    vitaminb12_in_mcg = "VITB12mcg",
    vitaminc_in_mg = "VITCmg",
    phytate_in_mg = "PHYTCPPDmg",
    nitrogen_in_g = "NTg", 
    cu_in_mg = "CUmg",
    mn_in_mcg = "MNmcg",
    i_in_mcg = "IDmcg",
    vitaminb6_in_mg = "VITB6mg_std",
    pantothenate_in_mg = "PANTACmg",
    biotin_in_mcg = "BIOTmcg",
    vitamind_in_mcg = "VITDmcg",
    vitamine_in_mg = "VITEmg",
    folicacid_in_mcg = "FOLACmcg"
    ) %>%
    select(var.name)

# Checking the data

#1) All foods that have a genus ID has a valid description (in dict.)
  
MAPS_output %>% filter(!is.na(food_genus_id) & 
                       is.na(food_genus_description)) %>% View()

dictionary.df %>% 
  filter(grepl("bread", FoodName_3) & 
           grepl("", FoodName_3)) %>% 
  select(FoodName_3, ID_3)


dictionary.df %>% 
  filter(ID_2 == "F0020") 

# Saving the MAPS_FCTs
   
 split_fct <- MAPS_output %>% 
  # filter(fct_name %in% c("UK21")) %>% 
     group_by(fct_name) %>% 
     group_split()
   
 fct_names <- unique(MAPS_output$fct_name)
#  fct_names <- c("UK21")
 
   
   for(i in 1:length(fct_names)){
     saveName = paste0("output/new/MAPS_", fct_names[i], "_v.3.1.1.csv")
     readr::write_excel_csv(split_fct[[i]], file = saveName)
   }
   
   
 


