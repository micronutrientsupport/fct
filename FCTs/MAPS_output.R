
# Scritp that format FCTs into MAPS standard name and structure

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()


# Rename variables according to MAPS-standards

MAPS_output <- fct %>%
  left_join(., dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 



### Other scripts
  
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


# Rename variables according to MAPS-standards

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

