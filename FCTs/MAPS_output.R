
# Script that format FCTs into MAPS standard name and structure

# Load libraries
library(dplyr)
source("functions.R")

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()

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


"FAT_g"                                           
"FATCEg"                                          
"FATg"   



"PHYTCPPD_PHYTCPPImg"                             
"PHYTCPPDmg"                                      
"PHYTCPPmg"                                       
"PHYTmg"                                          
"PHYTO"                                           
"Total PHYTO" - UK21



"TRPg"                                            
"TRPmg"



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

  
# Saving the MAPS_FCTs
  
split_fct <- MAPS_output %>% 
    group_by(fct_name) %>% 
    group_split()
  
fct_names <- unique(MAPS_output$fct_name)

  
  for(i in 1:length(fct_names)){
    saveName = paste0("output/MAPS_", fct_names[i], "_v2.0.0.csv")
    readr::write_excel_csv(split_fct[[i]], file = saveName)
  }
  
  
# Testing values


# Checking for duplicated items

dim(MAPS_ken)
which(duplicated(MAPS_ken))

MAPS_ken[which(duplicated(MAPS_ken)),]
subset(MAPS_ken, original_food_id == "2010")

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"]))
x <- which(duplicated(MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"]))

((MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"][x]))
subset(MAPS_ken, food_genus_id == "1290.9.14")

#Checking that all dictionary codes have been matched to an entry in the dictionary

subset(MAPS_ken, !is.na(food_genus_id) & is.na(food_genus_description))
MAPS_output %>% filter(!is.na(food_genus_id), is.na(food_subgroup))  

#Saving file into csv to be used in MAPS tool

fct_name <- unique(MAPS_output$source_fct)
verion <- "2.2"

file_name <-  paste0("MAPS_", fct_name, "_v", version, ".csv")
  
#readr::write_excel_csv(MAPS_ken, here::here('output', file_name))

