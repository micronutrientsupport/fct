
#Loading library, custom functions and data
library("tidyverse")
source("functions.R")
source("MAPS_Dictionary-Protocol.R")

#Customized saving FCT

LSOFCT <- readxl::read_excel(here::here("data", "2006_LSOFCT.xlsx"), 
                             sheet = 6) %>% mutate(FCT = 'LSOFCT') %>%  select(2:41) %>% glimpse()

FCT6_tag <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'WATER', 
              'CHOAVLDF', 'NT', 'PROTCNT', 'PROPLA', 'PROANI' , 'FIBTG', 'ASH', 'FAT',
              'CHOLE', 'FASAT', 'FAMS', 'FAPU', 'STARCH', 'SUGAR',
              'CA', 'FE', 'MG', 'P', 'K', 'NA', 'ZN','CU','SE', 'MN', 
              'VITA',  'CARBEQ', 'VITD', 'VITE', 
              'THIA', 'RIBF', 'NIA', 'VITB6'  ,'FOL', 'VITC', 'FCT')

LSOFCT <- LSOFCT %>% rename_all( ~ FCT6_tag) 

#creating variable 'foodgroups'

#Extracting variables names only in English

fglso <- LSOFCT %>% filter(is.na(code), !is.na(fooditem)) %>% pull(fooditem) %>%
  stringr::str_split_fixed( ' ', n = 2) %>% as_tibble()

nolso <- fglso %>%  pull(V1)

fglso <- fglso %>%  pull(V2)


LSOFCT <- LSOFCT %>% 
  mutate(foodgroup = ifelse(grepl(nolso[1], code), fglso[1],
                            ifelse(grepl(nolso[2], code), fglso[2],
                                   ifelse(grepl(nolso[3], code), fglso[3],
                                          ifelse(grepl(nolso[4], code), fglso[4], 
                                                 ifelse(grepl(nolso[5], code), fglso[5], 
                                                        ifelse(grepl(nolso[6], code), fglso[6], 
                                                               ifelse(grepl(nolso[7], code), fglso[7],
                                                                      ifelse(grepl(nolso[8], code), fglso[8], 
                                                                             ifelse(grepl(nolso[9], code), fglso[9],
                                                                                    ifelse(grepl(nolso[10], code), fglso[10], 
                                                                                           ifelse(grepl(nolso[11], code), fglso[11],
                                                                                                  ifelse(grepl(nolso[12], code), fglso[12], 
                                                                                                         ifelse(grepl(nolso[13], code), fglso[13], 
                                                                                                                ifelse(grepl(nolso[14], code), fglso[14],
                                                                                                                       ifelse(grepl(nolso[15], code), fglso[15],
                                                                                                                              ifelse(grepl(nolso[16], code), fglso[16],
                                                                                                                                     ifelse(grepl(nolso[17], code), fglso[17],  
                                                                                                                                            ifelse(grepl(nolso[18], code), fglso[18],
                                                                                                                                                   ifelse(grepl(nolso[19], code), fglso[19],
                                                                                                                                                          ifelse(grepl(nolso[20], code), fglso[20], 
                                                                                                                                                                 'NA')))))))))))))))))))))%>% 
  filter(!is.na(code))

# Replacing trace values to zero
LSOFCT[, c(3:39)] <- apply(LSOFCT[, c(3:39)],2, TraceToZero)
# Removing * values
LSOFCT[, c(3:39)] <- apply(LSOFCT[, c(3:39)],2, RemoveStar)

LSOFCT <- LSOFCT %>%  mutate_if(is.character, no_brackets) %>% 
  mutate_at(vars(3:39), funs(as.numeric)) 


LSOFCT <- LSOFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG',  'ASH'), `+`))


LSOFCT <- LSOFCT %>% rowwise %>%  mutate(ENERC1 = sum(c(
  (PROTCNT*4) , (FAT*9), (CHOAVLDF*4),(FIBTG *2))))


#Adding the reference (biblioID) to the main LSOFCT

ref.lso <- readxl::read_excel(here::here('data','2006_LSOFCT.xlsx'), 
                              sheet = 5) %>% 
  rename(ref = "FCT SOURCE") %>% select(code, ref) %>% 
  filter(!is.na(code))

LSOFCT<- LSOFCT %>% left_join(., ref.lso) 


##----Standardization of variables for MAPS

LSOFCT<- LSOFCT %>% rename(
  original_food_id = "code",
  original_food_name = "fooditem",
  fct_name = "FCT",
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
  se_in_mcg = "SE",
  mn_in_mcg = "MN", 
  vitamind_in_mcg = "VITD",
  vitamine_in_mg = "VITE",
  thiamin_in_mg = "THIA",
  riboflavin_in_mg = "RIBF", 
  niacin_in_mg = "NIA", 
  vitaminb6_in_mg = "VITB6",
  folate_in_mcg = "FOL",
  vitaminc_in_mg = "VITC") %>% 
  mutate_all(., as.character) %>% 
  left_join(., var.dat %>% mutate_all(., as.character)) %>% 
  select(var.name)

#Checking for duplicated items
dim(LSOFCT)
which(duplicated(LSOFCT))


##3) Adding GENuS code (1)
#and adjusting FCT formatting to MAPS standards

lso_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,             
  "30014",  "22290.01",    "l",
  "140001", "24310.04.01",  "m",  
  "140001", "24310.02.01",  "m")




MAPS_output <- LSOFCT %>% 
  #mutate_at("original_food_id", as.integer) %>% 
  left_join(., lso_genus, by = c("original_food_id" = "ref_fctcode")) %>% 
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  mutate(
    food_genus_id = ID_3,
    food_genus_description = FoodName_3,
    food_group = FoodName_0,
    food_subgroup = FoodName_1, 
    food_genus_confidence = confidence) %>% 
  select(original_food_id:phytate_in_mg) 

#Checking for duplicated items
dim(MAPS_output)
which(duplicated(MAPS_output))

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_output$food_genus_id[!is.na(MAPS_output$food_genus_id)]))

#Checking that all dictionary codes have been matched to an entry in the dictionary
subset(MAPS_output, !is.na(food_genus_id) & is.na(food_genus_description))

#Saving file into csv to be used in MAPS tool
readr::write_excel_csv(MAPS_output,
                       here::here('output', 'MAPS_LSOFCT_v1.4.csv'))