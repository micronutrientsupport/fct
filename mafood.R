

##) MAPS type format ------
##TO-DO: review data conversion from pdf to excel
#Loading data
mwi_clean <- read.csv(here::here("inter-output", "2019_MAFOODS_with-corrections.csv"))

#Loading formatting data
var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()
#Loading the food dictionary
source(here::here("MAPS_Dictionary-Protocol.R"))

#Checking dictionary codes
dictionary.df %>% filter(ID_3 == "01520.01.03")

mwi_clean %>% filter(code == "MW01_0031")

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
"MW03_0010" ,  "F1061.01", "l",
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
"MW08_0008" ,  "2899.01.01", "h",
"MW01_0031", "F1232.05", "l")

mwi_genus <-  mwi_genus %>% left_join(., dictionary.df)

#Adding genus variables and 
#Rename variables according to MAPS-standards

MAPS_output <- mwi_clean %>% 
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
  phytate_in_mg = "PHYT") %>% 
  select(var.name)

#Checking for duplicated items
dim(MAPS_output)
which(duplicated(MAPS_output))

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_output$food_genus_id[!is.na(MAPS_output$food_genus_id)]))

#Checking that all dictionary codes have been matched to an entry in the dictionary
subset(MAPS_output, !is.na(food_genus_id) & is.na(food_genus_description))

subset(MAPS_output, food_genus_id == "1341.01")
, 
       select = vitaminb12_in_mcg)

#Saving file into csv to be used in MAPS tool

#readr::write_excel_csv(MAPS_output,
 #        here::here('output', 'MAPS_MAFOODS_v1.6.csv'))


###========================= END =============================###

#MAPS_mwi <- read.csv(here::here('output', 'MAPS_MAFOODS_v1.5.csv'))

#MAPS_mwi %>% filter(str_detect(food_genus_description, "pig"))
