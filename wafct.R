



library(tidyverse)


##0) DOWNLOADING WEST-AFRICA FCT FROM FAO/INFOODS 
#
# Only need to do it the first time!
# 
#  f <- "http://www.fao.org/fileadmin/user_upload/faoweb/2020/WAFCT_2019.xlsx"
# 
#  download.file(f,"./data/INFOODS-WAFCT_2019.xlsx",
#               method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#               mode="wb")
# 

##1) LOADING WEST-AFRICA FCT 

#Check all the sheet in the spreadsheet
readxl::excel_sheets(here::here('data', 
                     'INFOODS-WAFCT_2019.xlsx'))


##View WAFCT structure
readxl::read_excel(here::here('data', 
                              'INFOODS-WAFCT_2019.xlsx'), sheet = 5) %>% head()


##Loading WAFCT, skip 2 first rows to use tagnames for components

wafct <- readxl::read_excel(here::here( 'data', 
                                        'INFOODS-WAFCT_2019.xlsx'), 
                            sheet = 5, skip = 2) %>%
  mutate(FCT = 'WAFCT') %>% glimpse()

##2) TIDYING WEST-AFRICA FCT 

#rename variables according to our standards

wafct <- wafct %>% rename(code = '...1', 
                          fooditem = '...2', 
                          fooditemFR = '...3',
                          scientificName = '...4',
                          ref = '...5' ,
                          ENERC2 = 'ENERC...9', 
                          ENERC1 = 'ENERC...10') 


#creating variable 'foodgroups'

#Extracting variables names only in English

fgwa <- wafct %>% filter(is.na(fooditem), !is.na(code)) %>% pull(code) %>%
  stringr::str_split_fixed( '/', n = 2) %>% as_tibble() %>% pull(V1)


wafct <- wafct %>% 
  mutate(foodgroup =
   ifelse(grepl("01_", code), fgwa[1],
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




#Creating a dataset w/ the values that were of low quality [] 
#trace or normal

#selecting nutrient variable names where we want to check for quality/trace
wa_nut <- wafct %>% select(EDIBLE1:XN) %>% colnames()

#dataset w/ metadata info that will be removed from the dataset for use
wa_meta_quality <- wafct %>% mutate_at(wa_nut,  ~case_when(
  str_detect(. , '\\[.*?\\]') ~ "low_quality", 
  str_detect(. , 'tr') ~ "trace",
  TRUE ~ "normal_value"))

wa_meta_quality %>% head()

#Extracting variables calculated with different (lower quality) method 
#and reported as using [] and removing them from the original variable


wafct <- wafct %>% 
  mutate(FATCE = str_extract(FAT, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FIBC = str_extract(FIBTG, '(?<=\\[).*?(?=\\])')) %>%
  mutate(CARTB = str_extract(CARTBEQ, '(?<=\\[).*?(?=\\])')) %>%
  mutate(TOCPHA = str_extract(VITE, '(?<=\\[).*?(?=\\])')) %>%
  mutate(NIA = str_extract(NIAEQ, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FOLSUM = str_extract(FOL, '(?<=\\[).*?(?=\\])')) %>%
  mutate(PHYTCPPD_PHYTCPPI = str_extract(PHYTCPP, '(?<=\\[).*?(?=\\])')) %>% 
  mutate_at(c("FAT", "FIBTG", "CARTBEQ", 
              "VITE", "NIAEQ", "FOL", "PHYTCPP"),
            ~ifelse(str_detect(. , '\\[.*?\\]') == TRUE, NA, 
                    .))


#The following f(x) removes [] and changing tr w/ 0

no_brackets_tr <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    TRUE ~ i)
}

wafct <- wafct %>% 
  mutate_at(wa_nut, no_brackets_tr)



#Reordering variables

wafct <- wafct %>% dplyr::relocate(foodgroup, .after = ref) %>%
  dplyr::relocate(FCT, .before = code) 


#Converting into numeric numeric variables  

wafct <- wafct %>% mutate_at(vars(`EDIBLE1`:`PHYTCPPD_PHYTCPPI`), as.numeric)

wafct %>% head()

##3) MAPS type format

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv'))

variables <- read.csv(here::here( "fct-variable-names.csv"))

source("dictionary.R")

wafct.genus %>% filter(ID_3 == "F0623.02")
wafct.genus %>% filter(ref_fctcode == "13_023")
WAFCT %>% filter(code == "12_012") %>% glimpse()

#check 24490 in dictionary
#add one for Juice, canned or bottled, sweetened (e.g. apple) == 12_012

wafct.genus %>%
  filter(!ID_3 %in% c("24230.03.02", "24230.03.03", "23110.01",
                      "1532.01", "23670.01.02", "24490.01", 
                      "23670.01.01", "F0623.02"))  %>% pull(ref_fctcode)


##3) Adding GENuS code (1)


wa_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "03_022", "1701.02", "h",
  "12_001", "24310.01.01", "m",
  "03_057", "1708.01", "h",
  "13_023", "F0623.01", "h",
  "01_095", "118.03", "h", 
  "10_002", "22211.01", "h",
  "12_024", "24490.02", "h",
  "13_021", "F0666.01", "h", 
  "07_063", "F1172.01", "m",
  "12_002",  "24310.02.01", "h", 
  "01_043", "23110.02", "h", 
  "12_012", "21435.01.01", "h",
  "02_003", "01520.01.02", "m",
  "02_001", "01520.01.01", "m",
  "02_015", "1550.01", "h",
  "02_081", "1510.02", "m",
  "01_004", "112.01", "m",
  "04_108", "1290.01.03", "m",
  "02_004", "23170.01", "m",
  "01_081", "23120.03.02", "l",
  "02_045", "1313.01", "l",
  "02_009", "1510.01", "h",
  "01_037",  "23161.01.01", "h",
  "01_039",  "114.01", "m",
  "02_049",  "1530.02", "h",
  "02_022",  "1530.01", "h",
  "02_014", "1530.04", "h",
  "03_004", "1706.02", "m",
  "06_027", "142.02", "m",
  "03_032", "1707.01", "h",
  "03_008", "141.01", "h",
  "07_103", "21121.03", "m",
  "07_030", "21121.01", "m",
  "08_002", "231.02", "h",
  "08_001", "231.01", "h",
  "14_001", "1533.01", "l",
  "09_043", "1501.03", "h",
  "07_046", "21116.01", "m",
  "10_029", "2211.01", "h",
  "07_072", "21115.01", "m",
  "07_006", "21113.02.01", "m",
  "07_007", "21114.01", "h",
  "04_005", "1212.01", "h",
  "04_053", "1214.04", "m",
  "04_017", "1239.01.01", "h",
  "04_018", "1253.02.01", "h", 
  "04_051", "1235.01", "m",
  "04_021", "1234.01", "m",
  "05_026", "1341.01", "m", 
  "05_002", "1311.01", "h",
  "05_003", "1312.01", "m",
  "05_010", "1316.02", "h",
  "05_037", "1316.01", "l",
  "05_017", "1317.01", "h",
  "05_018", "1318.01", "h")
  
#List of non-available items compared w/ mwi_genus
#F0022.04
#118.02 - only pearl millet
#23120.03.01
#F0022.02
#Check type of plantain
#Check type of sorghum
#1701.03
#141.02
#Check peanuts - all shelled
  #142.02 - check shelled?
  #142.05 - check shelled?
#21111.01.01 - check it said w/ bones
#21170.01.03
#1212.02
#Check leaves - fresh or dried??
#1214.03
#1270.01
#(1341.01) - apple here is w/ skin
#(1316.01) - check mangoes here conservative pale flesh
#21700.02.01 - margarine only fortified
#1802.01
#2899.01.01



wa_genus <- wa_genus %>% left_join(., dictionary)

#Rename variables according to MAPS-standards

  MAPS_wafct<- wafct %>%
  left_join(., wa_genus, by = c("code" = "ref_fctcode")) %>% 
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
  phyticacid_in_mg = "PHYTCPP")


  MAPS_wafct  %>% select(var.name) %>% 
  readr::write_excel_csv(., 
      here::here('output', 'MAPS_WAFCT_v1.3.csv')) #that f(x) is to 
        #deal w/ special characters 


