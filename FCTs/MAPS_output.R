
# Script that format FCTs into MAPS standard name and structure

# Load libraries
library(dplyr)
source("functions.R")

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()
# Adjusting variable names to updated version (V.1.6 updated on 2021/10/13)
var.name <- gsub("phyticacid", "phytate", var.name)

#Load dictionary-to-FCTs-matches (can be found in the fct_dict.R)
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

dict_comp <-read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")

dict_comp %>% count(source_fct) 

#1) Loading all FCDBs into one single database ----

#finding all the cleaned FCTs/FCDBs from the output folder
list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, #so it is not taking the fcts in the folder
           full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")))  

# Loading all the cleaned FCTs/FCDBs into one single object (data.frame)
fct_cover <- list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))) 

# checking that we have loaded all the FCT/FCDBs (n=5)
fct_cover %>% distinct(source_fct) 
colnames(fct_cover)

fct_cover$fdc_id[fct_cover$source_fct == "JA15"] <- gsub("^0", "", fct_cover$fdc_id )
fct_cover$fdc_id <- str_squish(fct_cover$fdc_id )
fct_cover$source_fct <- str_squish(fct_cover$source_fct )

# Merging dict codes to fcts codes

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3)) %>% count(source_fct)

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3) & source_fct == "WA19") %>% pull(fdc_id)

fct_dict <- fct_cover %>% left_join(., dict_comp)
#fct_dict <- merge(fct_cover, dict_comp)

dictionary.df %>% 
  filter(str_detect(ID_3, "\\b")) %>% 
  left_join(., fct_dict , by = "ID_3") %>% 
  filter(is.na(source_fct)) %>% select(FoodName_3, ID_3)


subset(fct_dict, grepl("baby|infant", food_desc, ignore.case = TRUE) &
         grepl("food", food_desc, ignore.case = TRUE) &
         grepl("", food_desc, ignore.case = TRUE),
       select = c(source_fct, fdc_id, food_desc, scientific_name, WATERg, ID_3)) 

sort(names(fct_dict))

fct_dict %>% filter(!is.na(PHYTCPPDmg)) %>% distinct(source_fct)

fct_dict <- fct_dict %>% 
  nia_conversion_creator()  %>%  # Standardisation of NIAmg
 THIAmg_std_creator() %>%  # Standardisation of THIAmg
  VITB6mg_std_creator()  # Standardisation of VITB6mg

 ## Standardisation: 





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
                       is.na(food_genus_description)) 

dictionary.df %>% 
  filter(grepl("bread", FoodName_3) & 
           grepl("", FoodName_3)) %>% 
  select(FoodName_3, ID_3)


dictionary.df %>% 
  filter(ID_2 == "F0020") 

# Saving the MAPS_FCTs
  
split_fct <- MAPS_output %>% 
  filter(fct_name %in% c("KE18", "MW19", "WA19")) %>% 
    group_by(fct_name) %>% 
    group_split()
  
fct_names <- unique(MAPS_output$fct_name)
fct_names <- c("KE18", "MW19", "WA19")

  
  for(i in 1:length(fct_names)){
    saveName = paste0("output/MAPS_", fct_names[i], "_v2.0.1.csv")
    readr::write_excel_csv(split_fct[[i]], file = saveName)
  }
  
  



