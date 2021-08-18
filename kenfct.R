

library(tidyverse)


##0) DOWNLOADING KENYA FCT FROM HUMAN NUTRION AND DIETETICS UNIT, MOH, KENYA

#Only need to do it once!

f <- "http://www.nutritionhealth.or.ke/wp-content/uploads/Downloads/Kenya%20Food%20Composition%20Tables%20Excel%20files%202018.xlsx"

download.file(f,"./data/MOH-KENFCT_2018.xlsx",
              method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
              mode="wb")


##1) LOADING PACKAGES AND KENYA FCT 

library(tidyverse)

#Check all the sheet in the spreadsheet
readxl::excel_sheets(here::here('data', "MOH-KENFCT_2018.xlsx"))

readxl::read_excel(here::here('data', "MOH-KENFCT_2018.xlsx"), sheet = 4, skip = 2) %>%
  tail()


#Customized saving FCT

kenfct <- readxl::read_excel(here::here('data', "MOH-KENFCT_2018.xlsx"),
                             sheet = 4, skip = 2) %>%
  mutate(FCT = 'KENFCT') %>% #adding a column with the FCT short-name
  slice(1:1240) %>%   #removing last rows that are empty only provide notes info
  glimpse()

##2) TIDYING KENYA FCT 

#Rename variables acc. to tagnames (FAO/INFOODS)
#We are not renaming Fatty acids nor AAs

ken_names <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'ENERC1', 'WATER', 
              'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG', 'ASH', 
              'CA', 'FE', 'MG', 'P', 'K', 'NA.', 'ZN', 'SE',
              'VITA_RAE', 'VITA', 'RETOL', 'CARBEQ', 
              'THIA', 'RIBF', 'NIA', 'FOLDFE', 'FOLFD',
              'VITB12', 'VITC', 'CHOLE', 'OXALAC', 'PHYTCPPD', 'IP3', 'IP4',
              'IP5', 'IP6','FASAT', "FAMS","FAPU", 'FCT')

kenfct <- kenfct %>% rename_at(vars(1:37, 60:62, 320),  ~ken_names) 

#creating variable 'foodgroups'

kenfg <- kenfct %>%  filter(code %in% c(1:15)) %>% pull(fooditem)

kenfct <- kenfct %>% mutate(foodgroup = case_when(
  str_detect(code, "[:digit:]{5}") & str_starts(code, '10') ~ kenfg[10],
  str_starts(code, '10') ~ kenfg[1],
  str_starts(code, '20') ~ kenfg[2],
  str_starts(code, '30') ~ kenfg[3],
  str_starts(code, '40') ~ kenfg[4],
  str_starts(code, '50') ~ kenfg[5],
  str_starts(code, '60') ~ kenfg[6],
  str_starts(code, '70') ~ kenfg[7],
  str_starts(code, '80') ~ kenfg[8],
  str_starts(code, '90') ~ kenfg[9],
  str_starts(code, '11') ~ kenfg[11],
  str_starts(code, '12') ~ kenfg[12],
  str_starts(code, '13') ~ kenfg[13],
  str_starts(code, '14') ~ kenfg[14],
  str_starts(code, '15') ~ kenfg[15])) %>% 
  filter(!is.na(ENERC1), !is.na(fooditem)) #Removing NA, SD/min-max

###############################################################################
# kenfct %>% filter(str_detect(fooditem, "[:upper:]{5,}"))                    #
#                                                                             #
# kenfct %>%  filter(str_detect(code, "[:digit:]{4,5}")) %>% distinct(code)   #
#                                                                             #
#                                                                             #
# kenfct %>%  filter(str_detect(code, "[:digit;]{4,5}"))                      #
#                                                                             #    
###############################################################################


#Creating a dataset w/ the values that were of low quality [],  
#trace, fortified w/ folic acid or normal

ken_meta_quality <- kenfct %>% 
  mutate_at(vars(EDIBLE:`Fatty acid 24:6 (/100 g FA)`),  ~case_when(
  str_detect(. , '\\[.*?\\]') ~ "low_quality", 
  str_detect(. , '[*]') ~ "folic-fortified", 
  str_detect(. , 'tr') ~ "trace",
  TRUE ~ "normal_value"))

#codes of the items identified as fortified with folic acid
folac <- kenfct %>% filter(str_detect(FOLDFE, '[*]')) %>% pull(code) 

#Extracting variables calculated with different (lower quality) method 
#and reported as using [] and removing * from FOLDFE
#and changing tr w/ 0

no_brackets_tr_ast <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    str_detect(i, '[*]')  ~ str_extract(i, "[:digit:]+"),
    TRUE ~ i)
}

kenfct <- kenfct %>% 
  mutate_at(vars(EDIBLE:`Fatty acid 24:6 (/100 g FA)`), no_brackets_tr_ast)

#Check that all tr, [] and * are removed 
#NOTE: tr will be found in non-numeric variables (i.e., fooditem)
kenfct %>% str_which(.,"tr|[tr]|[*]|\\[.*?\\]")


#Adding the reference (biblioID) and Scientific name to kenfct

kenfct <- kenfct %>% left_join(., readxl::read_excel(here::here('data', "MOH-KENFCT_2018.xlsx"), 
                   sheet = 7, skip = 2) %>%
                  janitor::clean_names() %>% 
                  select(2, 4,5) %>% 
                  mutate_at("code_kfct18", as.character),
                  by = c("code" = "code_kfct18")) 

  
#Reordering variables and converting nutrient variables into numeric

kenfct <- kenfct %>% dplyr::relocate(c(scientific_name, foodgroup, biblio_id),
                                     .after = fooditem) %>%
  dplyr::relocate(FCT, .before = code) %>% 
  mutate_at(vars(EDIBLE:`Fatty acid 24:6 (/100 g FA)`), as.numeric)

kenfct %>% head()

#Then convert to numeric to calculate FOLAC

kenfct %>%  
  mutate(FOLAC = (FOLDFE-FOLFD)/1.7) %>% select(code, FOLAC) %>% 
  filter(!code %in% folac, FOLAC> 0) %>% arrange(FOLAC) %>% knitr::kable()

#Detecting FOLAC (folic acid used in fortified food)
#15065 - Fortified. Misreported in excel but reported in pdf
#15019 - It seems fortified, although it's not reported as such. 

###=======================DONT RUN==========================================####
#   kenfct %>% filter(str_detect(FOLDFE, '[*]')) %>% pull(code, FOLDFE)        
#   
#   
#   
#   kenfct %>% select(-fooditem) %>%  
#     str_subset(., "[*]") 
#   
#   #Use function to remove bracket and create a new variable 
#   #to calculate the fortification from FOLDFE
#   
#   kenfct %>%  
#     mutate(FOLAC = ifelse( 
#    str_detect(FOLDFE, '[*]'), str_extract(FOLDFE, "[:digit:]+"), FOLDFE)) %>% 
#      mutate_at(c("FOLAC", "FOLFD"), as.numeric) %>% 
#      mutate(FOLAC = (FOLAC-FOLFD)/1.7)
###############################################################################   

##3) MAPS type format

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv'))

var.name <- read.csv(here::here("fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()

source("dictionary.R")

## Adding GENuS code (1)

ken.genus %>% count(ref_fctcode) %>% arrange(desc(n))
#       13031 2
#        15026 2

kenfct %>% filter(code == "6026") %>% pull(fooditem)
dictionary %>% filter(ID_3 == "22230.01.01")

ken_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "1004",   "F0022.03",    "h",
  "6001",   "22241.01.01", "h",
  "12004",  "F0665.01",    "m", 
  "12003",  "23912.02.01", "h", 
  "4016",   "1232.01"  ,   "h",
  "11001",  "2910.01"  ,   "h",
  "10010",  "1379.9.01" ,  "h", 
  "13027",  "1699.02"   ,  "m", #salt, ionized vs salt
  "15026",  "F0022.05"  ,  "h",
  "1031",   "23710.01"  ,  "h", 
  "13028",  "1699.03"   ,  "h",
  "11003",  "23520.01"  ,  "h",
  "12005",  "23914.02"  ,  "m",
  "13031",  "21399.01.01" ,"m",
  "12008",  "24212.02.01" ,"h",
  "6026",   "22230.01.01" ,"h")

ken_genus <- ken_genus %>% left_join(., dictionary)


#Rename variables according to MAPS-standards

MAPS_ken <- kenfct %>% 
  left_join(., ken_genus, by = c("code" = "ref_fctcode")) %>%   
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
  phyticacid_in_mg = "PHYTCPPD") %>% 
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


MAPS_ken %>% head()

readr::write_excel_csv(., 
                       here::here('output', 'MAPS_KENFCT_v1.2.csv'), #that f(x) is to 
                       row.names = FALSE)   