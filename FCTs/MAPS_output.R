
# Script that format FCTs into MAPS standard name and structure

# Load libraries
library(dplyr)

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


subset(fct_dict, grepl("", food_desc, ignore.case = TRUE) &
         grepl("", food_desc, ignore.case = TRUE) &
         grepl("refined", food_desc, ignore.case = TRUE),
       select = c(source_fct, fdc_id, food_desc, scientific_name, WATERg, ID_3))


## Checking

# Load SUA data

sua <- read.csv(here::here("inter-output", "MAPS_SUA_v1.0.0.csv"))
names(sua)

sua %>% left_join(., fct_dict, by = "ID_3") %>% 
  filter(!is.na(WATERg)) %>% distinct(Item)


# Rename variables according to MAPS-standards

MAPS_output <- fct %>%
  left_join(., dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 



### Other scripts 
  
#KE18
MAPS_ken <- kenfct %>% 
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>%   
  rename(
    original_food_id = "code",
    original_food_name = "fooditem",
    food_genus_id = "ID_3",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1", 
    food_genus_confidence = "confidence",
    fct_name = "FCT",
    data_reference_original_id = "biblio_id",
    moisture_in_g = "WATER",
    energy_in_kcal = "ENERC1",
    energy_in_kj = "ENERC2",
    totalprotein_in_g = "PROTCNT",
    totalfats_in_g = "FAT",
    saturatedfa_in_g = "FASAT", 
    monounsaturatedfa_in_g = "FAMS", 
    polyunsaturatedfa_in_g = "FAPU", 
    cholesterol_in_mg = "CHOLE",
    carbohydrates_in_g = "CHOAVLDF", 
    fibre_in_g = "FIBTG", 
    ash_in_g = "ASH",
    ca_in_mg = "CA", 
    fe_in_mg = "FE",
    mg_in_mg = "MG",
    p_in_mg = "P",
    k_in_mg = "K",
    na_in_mg = "NA.", 
    zn_in_mg = "ZN",
    se_in_mcg = "SE",
    vitamina_in_rae_in_mcg = "VITA_RAE", 
    thiamin_in_mg = "THIA",
    riboflavin_in_mg = "RIBF", 
    niacin_in_mg = "NIA", 
    folate_in_mcg = "FOLFD",
    vitaminb12_in_mcg = "VITB12",
    vitaminc_in_mg = "VITC",
    phytate_in_mg = "PHYTCPPD") %>% 
  mutate(
    nitrogen_in_g = "NA", 
    cu_in_mg = "NA",
    mn_in_mcg = "NA",
    i_in_mcg = "NA",
    vitaminb6_in_mg = "NA",
    pantothenate_in_mg = "NA",
    biotin_in_mcg = "NA",
    vitamind_in_mcg = "NA",
    vitamine_in_mg = "NA",
    folicacid_in_mcg = "NA") %>% select(var.name)

#WA19
MAPS_output <- fct %>%
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  mutate(nitrogen_in_g = NA, 
         mn_in_mcg = NA,
         i_in_mcg = NA, 
         se_in_mcg = NA, 
         pantothenate_in_mg = NA, 
         biotin_in_mcg = NA) %>% 
  rename(
    original_food_id = "code",
    original_food_name = "fooditem",
    fct_name = "FCT",
    food_genus_id = "ID_3",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1", 
    food_genus_confidence = "confidence",
    data_reference_original_id = "ref",
    moisture_in_g = "WATER",
    energy_in_kcal = "ENERC1",
    energy_in_kj = "ENERC2", 
    totalprotein_in_g = "PROTCNT",
    totalfats_in_g = "FAT",
    saturatedfa_in_g = "FASAT", 
    monounsaturatedfa_in_g = "FAMS", 
    polyunsaturatedfa_in_g = "FAPU", 
    cholesterol_in_mg = "CHOLE",
    carbohydrates_in_g = "CHOAVLDF", 
    fibre_in_g = "FIBTG", 
    ash_in_g = "ASH",
    ca_in_mg = "CA", 
    fe_in_mg = "FE",
    mg_in_mg = "MG",
    p_in_mg = "P",
    k_in_mg = "K",
    na_in_mg = "NA", 
    zn_in_mg = "ZN", 
    cu_in_mg = "CU", 
    vitamina_in_rae_in_mcg = "VITA_RAE", 
    thiamin_in_mg = "THIA",
    riboflavin_in_mg = "RIBF", 
    niacin_in_mg = "NIA", 
    vitaminb6_in_mg = "VITB6C", 
    folicacid_in_mcg = "FOLAC", 
    folate_in_mcg = "FOL",
    vitaminb12_in_mcg = "VITB12", 
    vitaminc_in_mg = "VITC",
    vitamind_in_mcg = "VITD",
    vitamine_in_mg = "VITE", 
    phytate_in_mg = "PHYTCPP") %>% 
  select(var.name)

#UK21

%>% 
  mutate_at(c("RETOLmcg", "CARTBEQmcg"), as.numeric) %>% 
  VITA_RAEmcg_std_creator() %>% 
  rename(
    original_food_id = "fdc_id",
    original_food_name = "food_desc",
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
    nitrogen_in_g = "NTg", 
    totalfats_in_g = "FAT_g",
    saturatedfa_in_g = "FASATg", 
    monounsaturatedfa_in_g = "FAMSg", 
    polyunsaturatedfa_in_g = "FAPUg", 
    cholesterol_in_mg = "CHOLmg",
    carbohydrates_in_g = "CHOAVLg", 
    fibre_in_g = "FIBTGg", 
    ca_in_mg = "CAmg", 
    fe_in_mg = "FEmg",
    mg_in_mg = "MGmg",
    p_in_mg = "Pmg",
    k_in_mg = "Kmg",
    na_in_mg = "NAmg", 
    zn_in_mg = "ZNmg",
    se_in_mcg = "SEmcg",
    i_in_mcg = "IDmcg",
    cu_in_mg = "CUmg",
    mn_in_mcg = "MNmg",
    vitamina_in_rae_in_mcg = "VITA_RAEmcg_std", #Re-calculated
    thiamin_in_mg = "THIAmg",
    riboflavin_in_mg = "RIBFmg", 
    niacin_in_mg = "NIAmg", 
    folate_in_mcg = "FOLmcg", # This should be reviewed
    vitaminb12_in_mcg = "VITB12mcg",
    vitaminc_in_mg = "VITCmg",
    phytate_in_mg = "Total.PHYTO", 
    vitaminb6_in_mg = "VITB6_mg",
    pantothenate_in_mg = "PANTACmg",
    biotin_in_mcg = "BIOTmcg",
    vitamind_in_mcg = "VITDmcg",
    vitamine_in_mg = "VITEmg") %>% 
  mutate(
    folicacid_in_mcg = "NA", 
    ash_in_g = "NA") %>% select(var.name)



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

