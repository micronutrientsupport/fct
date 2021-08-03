



library(tidyverse)


##1) DOWNLOADING WEST-AFRICA FCT FROM FAO/INFOODS 

f <- "http://www.fao.org/fileadmin/user_upload/faoweb/2020/WAFCT_2019.xlsx"

download.file(f,"./data/INFOODS-WAFCT_2019.xlsx",
              method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
              mode="wb")



##View WAFCT structure

readxl::read_excel(here::here('data', 
                              'INFOODS-WAFCT_2019.xlsx'), sheet = 5) %>% head()


##Loading WAFCT, skip 2 first rows to use tagnames for components

WAFCT <- readxl::read_excel(here::here( 'data', 
                                        '2019_WAFCT.xlsx'), sheet = 5, skip = 2) %>%
  mutate(FCT = 'WAFCT') %>% glimpse()


#rename variables according to our standards

WAFCT <- WAFCT %>% rename(code = '...1', 
                          fooditem = '...2', 
                          fooditemFR = '...3',
                          scientificName = '...4',
                          ref = '...5' ,
                          ENERC2 = 'ENERC...9', 
                          ENERC1 = 'ENERC...10') 


#creating variable 'foodgroups'

#Extracting variables names only in English

fgwa <- WAFCT %>% filter(is.na(fooditem), !is.na(code)) %>% pull(code) %>%
  stringr::str_split_fixed( '/', n = 2) %>% as_tibble() %>% pull(V1)


WAFCT <- WAFCT %>% 
  mutate(foodgroup = ifelse(grepl("01_", code), fgwa[1],
   ifelse(grepl("02_", code), fgwa[2],
   ifelse(grepl("03_", code), fgwa[3], 
   ifelse(grepl("04_", code), fgwa[4], 
   ifelse(grepl("05_", code), fgwa[5],
   ifelse(grepl("06_", code), fgwa[6],
   ifelse(grepl("07_", code), fgwa[7],
   ifelse(grepl("08_", code), fgwa[8],
   ifelse(grepl("09_", code), fgwa[9], 
   ifelse(grepl("10_", code), fgwa[10], 
   ifelse(grepl("11_", code), fgwa[11],
   ifelse(grepl("12_", code), fgwa[12],     
   ifelse(grepl("13_", code), fgwa[13],
   ifelse(grepl("14_", code), fgwa[14],
          'NA'))))))))))))))) %>% 
   filter(!is.na(fooditem))

#Extracting variables calculated with different method and reported as using []


#This keeps [] 

#WAFCT <- WAFCT %>% mutate(FIBC = str_extract(FIBTG, "\\[.*?\\]"))


#This keeps only numbers

WAFCT <- WAFCT %>% 
  mutate(FIBC = str_extract(FIBTG, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FATCE = str_extract(FAT, '(?<=\\[).*?(?=\\])')) %>%
  mutate(TOCPHA = str_extract(VITE, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FOLSUM = str_extract(FOL, '(?<=\\[).*?(?=\\])')) %>%
  mutate(PHYTCPPD_I = str_extract(PHYTCPP, '(?<=\\[).*?(?=\\])'))


#This keeps numbers and create a variable with low_quality

a <- WAFCT %>% mutate(low_quality_FE = case_when(
  str_detect(FE, '\\[.*?\\]') == TRUE ~ 'yes', 
  TRUE ~ 'no')) 

#The following f(x) removes []

no_brackets <- function(i){
  case_when(
    str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'), 
    TRUE ~ i)
}

WAFCT <- WAFCT %>% mutate_if(is.character, no_brackets)


#Reordering variables

WAFCT <- WAFCT %>% dplyr::relocate(foodgroup, .after = ref) %>%
  dplyr::relocate(FCT, .before = code)

#WE don't need this bit anymore
#WAFCT <- WAFCT[,c(64, 1:4, 65, 5:69)]


#Converting into numeric numeric variables  

WAFCT<- WAFCT %>% mutate_at(vars(8:69), funs(as.numeric)) 


#calculating Ash for oils

WAFCT %>% filter(is.na(ASH))

WAFCT<- WAFCT %>% rowwise %>% mutate( CL_cal = `NA`* 2.5)

WAFCT<- WAFCT %>% mutate(               
  ASH_cal = ifelse(is.na(ASH), TRUE, FALSE),
  ASH = ifelse(is.na(ASH), 
               reduce(select(., 'CA', 'FE',  'CL_cal',
                             'MG',  'P', 'K', 'NA', 'ZN',
                             'CU')/1000, `+`), ASH))


#There is no need to calculate SOP for WAFCT

summary(WAFCT$SOP)



##2) Rename variables according to MAPS-standards

WAFCT<- WAFCT %>% rename(
  original_food_id = "code",
  original_food_name = "fooditem",
  fct_name = "FCT",
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
  phyticacid_in_mg = "PHYTCPP")


WAFCT<- WAFCT %>% left_join(., var.dat) %>% select(var.name)

#We use this one to correctly import 'strange characters' (i.e. french accents)

readr::write_excel_csv(WAFCT,  here::here('data', 'MAPS_WAFCT_v1.1.csv'))

##3) Adding GENuS code (1)
#and adjusting FCT formatting to MAPS standards

WAFCT <- read.csv(here::here('data', 'MAPS_WAFCT_v1.1.csv'), 
                  fileEncoding = 'UTF-8-BOM')

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv'))

WAFCT %>% left_join(., wafct.genus, by = c("original_food_id" = "ref_fctcode")) %>% 
  mutate(
    food_genus_id = ID_3,
    food_genus_description = FoodName_3,
    food_group = FoodName_0,
    food_subgroup = FoodName_1, 
    food_genus_confidence = fe2_confidence) %>% 
  select(original_food_id:phyticacid_in_mg) %>% 
  write.csv(here::here('output', 'MAPS_WAFCT_v1.2.csv'), row.names = FALSE)