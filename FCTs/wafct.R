
################################################################################
#
#                           FAO/INFOODS
#        Western Africa Food Composition Table (WAFCT, 2019)
#
#
#
################################################################################

# 0) DOWNLOADING WEST-AFRICA FCT FROM FAO/INFOODS 
#
# Only need to do it the first time!
# 
#  f <- "http://www.fao.org/fileadmin/user_upload/faoweb/2020/WAFCT_2019.xlsx"
# 
#  download.file(f,"./data/INFOODS-WAFCT_2019.xlsx",
#               method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#               mode="wb")
# 

# 0) Loading r packages

library(tidyverse)

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

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
                          scientific_name = '...4',
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


# Converting into numeric numeric variables  

wafct <- wafct %>% mutate_at(vars(`EDIBLE1`:`PHYTCPPD_PHYTCPPI`), as.numeric)

wafct %>% head()

##3) MAPS type format ----

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv'))

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()


#check 24490 in dictionary
#add one for Juice, canned or bottled, sweetened (e.g. apple) == 12_012

wafct.genus %>%
  filter(!ID_3 %in% c("24230.03.02", "24230.03.03", "23110.01",
                      "1532.01", "23670.01.02", "24490.01", 
                      "23670.01.01", "F0623.02"))  %>% pull(ref_fctcode)


##3) Adding GENuS code (1)


wa_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  #"03_022", "1701.02", "h",
  "12_001", "24310.01.01", "m",
  "03_057", "1708.01", "h",
  "13_023", "F0623.01", "m",
  "01_095", "118.03", "h", 
  "10_002", "22211.01", "h",
 # "12_024", "24490.02", "h",
  "13_021", "F0666.01", "h", 
  "07_063", "F1172.01", "m",
  "12_002",  "24310.02.01", "h", 
  "01_043", "23110.02", "h", 
  "12_012" , "21439.9.01", "m",
  "02_003", "1520.01.02", "m",
  "02_001", "1520.01.01", "m",
  "02_015", "1550.01", "h",
  "02_081", "1510.02", "m",
  "01_004", "112.01", "m",
  "04_108", "1290.01.03", "m",
  "02_004", "23170.01", "m",
  "01_081", "23120.03.02", "l",
  "02_045", "1313.01", "l",
  "02_009", "1510.01", "h",
  "01_037",  "23161.01.02", "l", # rice, white, imported, polished see docu!
  "01_039",  "114.01", "m",
  "02_049",  "1530.02", "h",
  "02_022",  "1530.01", "h",
  "02_014", "1530.04", "h",
  "03_004", "1706.01", "h",
  "06_027", "142.02", "m",
  "03_032", "1707.01", "h",
  "03_008", "141.01", "h",
  "07_003", "21121.03", "m",
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
  "04_005", "1212.03", "h",
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
  "05_018", "1318.01", "h", 
  "11_007", "2165.01", "l", 
  "01_101",  "23140.07.01", "m", 
   "04_011", "1699.08", "m", 
  "04_003", "1241.9.02", "h",
  "09_004", "1529.02", "h",
  "02_002", "1520.02.01", "h",
 "09_053", "1505.00.01", "l",
 "09_007", "1503.04", "h", 
  "09_015", "1503.05", "h",
 "09_018", "1503.06", "h",
 "09_060", "1503.07", "h",
 "09_032", "1503.02", "h",
 "09_041", "1503.08", "h",
 "01_074", "111.01", "h", 
 "01_082", "23110.01", "h", 
 "01_079", "23120.01.01", "h",
 "01_001",  "1193.03", "h", 
 "02_019", "1540.01", "l", 
 "13_001", "2910.01", "h",
 "03_030", "1704.01", "h", 
 "06_001", "1372.01", "m", 
 "06_010", "21421.01", "h", 
 "06_002", "1460.01", "h",
 "06_015", "1444.01", "m", 
 "11_009", "2161.01", "m", 
 "11_003", "2162.01", "m", 
 "11_016", "21631.01.01", "m", 
 "11_012", "21691.14.01", "m", 
 "11_002", "2166.01", "m", 
 "11_014", "2167.01", "m", 
 "11_013", "21691.02.01", "m", 
 "11_010", "34550.01", "m", 
 "05_016", "1323.01", "h", 
 "05_014", "1322.01", "h", 
 "05_035", "1321.02", "l",
 "05_031", "1314.01", "h", 
 "05_051", "1330.01", "h", 
 "12_005", "23912.02.01", "h", 
 "12_008", "23914.01", "h", 
 "13_014", "1651.01", "m", #Should specified dried 
 "04_046", "1652.01", "l", #should specidied is "chilli"
 "12_004", "24310.04.01", "h",
 "07_009", "21111.02.01", "m", #assumed w/o bones bc EP was 1
 "07_001", "21151.02", "h", 
 "11_015", "22241.02.01", "h", 
 "09_055", "1553.01", "l", 
 "09_006", "1514.01", "h", 
 "09_018", "1501.02", "m", 
"09_003",  "1527.01", "l", #need to check w/ specie
 "09_037", "1532.01", "l", #need specify in oil
 "06_029", "1491.02.01", "l", 
 "01_163", "23120.01.02", "l", 
"04_002", "21393.9.01", "h",
"04_081", "21393.9.02", "h",
"04_031", "21393.9.03", "h",
"04_014", "21393.9.04", "h",
"04_075", "21393.9.05", "h",
"04_077", "21393.9.06", "h",
"04_044", "21393.9.07", "h",
"04_055", "21393.9.08", "h",
"04_071", "21393.9.09", "h",
"04_062", "21393.9.10", "h",
"04_083", "21393.9.11", "h",
"09_057", "1555.01", "h",
"07_019", "21155.03", "h",
"07_021", "21155.04", "h", 
"07_053", "21155.01", "h", 
"07_051", "21155.02", "h", 
"07_039", "21160.01.01", "h",
"07_041", "21160.01.02", "h",
"07_017", "21182.01", "h", 
"07_027", "21183.01", "h",
"07_044", "21170.92.04", "h",
"13_008", "F1232.17", "h", 
"03_154", "F1232.15", "h", 
 "04_0162", "1290.9.12", "h",
 "05_011", "1359.9.03", "h",
"10_016",  "22221.01.01", "h", 
"01_034", "23162.03", "h", 
"01_065", "23162.04", "h", 
"01_067", "23161.01.04", "h",
"01_036", "23161.01.01", "l",
"01_045" , "F0022.09", "h", 
#"01_187",  "F0020.07", "l", # This is cake not sweet bread (can use value from KE18)
"01_189" , "F0022.14", "h", 
"04_012",  "1233.01", "h" ,
"04_074",  "1290.9.09", "h",
"03_005" , "1706.03", "h",
"03_027",  "1706.03", "h",
"03_006",  "1706.03", "h",
"02_041",  "23170.01.02", "h", 
"02_040",  "23170.01.03", "h",
"07_014",  "21111.02.03", "m",
 "07_002", "21111.02.02", "m", 
"02_084",  "1313.03", "h", 
"01_041",  "114.02", "h", 
"01_040",  "114.03", "h", 
"01_099",  "F1232.16", "m", 
"13_006", "1652.02", "m",
 "12_020", "24310.01.02", "h",
"01_182", "23140.03.01", "m", 
"10_018", "2293.01", "h", 
"10_003", "2292.01", "h", 
"10_023", "2291.01", "h",
"09_001", "1529.07", "h",
"01_100",  "F1232.17", "h",
"01_168",  "F1232.05", "m", 
"02_039", "23170.01.04", "h",
"02_005", "1591.01", "h", 
"02_043", "1591.02", "h", 
"04_008", "1219.01.01", "h", 
"05_012", "1359.9.01", "h", 
"05_004", "1359.9.02", "h",
"04_001", "21393.9.01", "h",
"12_009", "23912.02.02", "m", 
"13_017", "F1232.06", "h", 
"14_033", "F1232.18", "h", 
"14_034", "F1232.19", "h", 
"06_039", "1449.9.02", "h",
"06_013", "1449.01.01", "h", 
"01_046", "F0020.01", "m",
"11_011", "22241.01.02", "h", 
"11_001", "22241.01.01", "m", #Assumed standard is unsalted
"10_015", "22222.01.01", "h", 
"04_082", "1657.01", "h", 
"04_023", "1215.02", "h", 
"04_004", "1239.01.02", "h", 
"09_002", "1529.01", "h", 
"09_049", "1529.05", "h", 
"09_050", "1529.06", "h", 
"09_071", "1533.09", "h", 
"09_014", "1533.10", "h", 
"09_074", "1533.11", "h",
"07_066", "21170.92.06", "h",
"07_067", "21170.92.05", "h",
"02_093", "F1232.20", "m", 
"02_091", "F1232.21", "m",
"02_092" ,"F1232.22", "m",
"09_038", "1516.01", "h",
"09_052", "1516.02", "h",
"09_084", "1520.08", "h",
"09_040", "1520.09", "h",
"09_047", "1520.10", "h",
"04_040", "1214.01", "h", 
"09_058", "1553.02", "m", 
"05_054", "1319.05", "h", 
"05_050", "1319.06", "h",
"06_026", "21495.02.01", "h", 
"06_018", "1379.02", "h",
"01_052", "23710.02", "h",
"04_066", "21399.01.02", "h", 
"01_035", "23162.02", "h", 
"01_073",  "39120.01.01", "h",
"07_023", "21184.01.02", "h",
"10_012", "23991.01.04", "h", 
"10_011",  "23991.01.03", "h",
"13_024", "F1232.03", "h", 
"12_025", "141.03", "h", 
"12_013", "21439.9.02", "h", 
"09_025", "1533.03", "h", 
"10_025", "22230.01.02", "h", 
"10_026", "22230.02.01", "h",
"02_021", "1520.02.02", "h", 
 "12_014", "21439.9.12", "h",
"10_006", "22251.01.02", "h", 
"10_028", "22251.01.05", "h", 
"10_008", "22251.01.06", "h", 
"10_007", "22254.01", "h", 
"10_027", "22251.01.07", "h",
"09_005", "1529.04", "h", 
"09_110", "1532.02", "h", 
"09_111", "1532.03", "h", 
"08_009", "232.01", "h", 
"08_008", "232.02", "h", 
"08_006", "232.03", "h", 
"08_007", "232.04", "h", 
"05_033", "21419.02.01", "h", 
"05_029", "1324.03", "h", 
"01_002", "1193.01", "h", 
 "01_050", "1193.04", "h", # used white bc it's same specie (scientific name)
)

# Checking for dictionary duplicates -----

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv')) %>% 
 filter(!ref_fctcode %in%
        c("01_043", "07_063","10_002", "12_002",
          "12_012", "13_021", "13_023")) %>% #removing dupli
  select(ref_fctcode, ID_3, fe2_confidence) %>%
  mutate_at("ref_fctcode", as.character) %>% 
  rename(confidence = "fe2_confidence") %>% 
  bind_rows(wa_genus) %>% distinct()

wafct.genus %>%  count(ref_fctcode) %>% 
  filter(n>1) 
                          
subset(wafct.genus, ref_fctcode == "05_011")

#List of non-available items compared w/ mwi_genus

#F0022.04 = scones 
#118.02 - only pearl millet
#23120.03.01 = maize, flour, white, unrefined, non-fermented, raw
#F0022.02 = dough, sweet, fried?
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


#Checking the items of the list above

dictionary.df %>% 
  filter(ID_3 %in% c("F0022.04", "118.02", "23120.03.01", 
                       "F0022.02", "1701.03", "141.02", 
                       "142.02", "142.05", "21111.01.01",
                       "21170.01.03", "1212.02", "1214.03", 
                       "1270.01", "1341.01", "1316.01", 
                       "21700.02.01", "1802.01","2899.01.01"))


#wa_genus <- wa_genus %>% left_join(., dictionary.df)

#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

wafct.genus %>% mutate(fct = "WA19")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at("ref_fctcode", as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding dictionary code

wafct <- wafct %>%
  left_join(., wafct.genus, by = c("code" = "ref_fctcode"))

# Checking dictionary codes
wafct.genus %>% filter(ID_3 == "")
wafct.genus %>% filter(ref_fctcode == "05_011")
wafct %>% filter(code == "02_021") %>% glimpse()

#Checking code availability 
wafct %>% filter(code %in% c("12_012", "12_013")) %>% View()

subset(wafct, code %in% c("09_024", "09_025", "09_069" ), 
       select = c(code, fooditem, ID_3, scientific_name))

subset(wafct, code == "09_004", select = fooditem) 
subset(wafct, code == "09_001", select = c(fooditem, ID_3, scientific_name)) 
subset(wafct, ID_3 == "141.03") 
subset(wafct, str_detect(ID_3, "01520"))

dictionary.df %>% filter(ID_3 %in% c("1529.03", "1527.03"))
subset(dictionary.df, ID_2 == "1529")
subset(dictionary.df, ID_1 == "2645")
subset(dictionary.df, ID_0 == "AP")

distinct(subset(dictionary.df,
            ID_1 == "2605", select = FoodName_2))


subset(wafct, grepl("fonio", fooditem, ignore.case = TRUE) &
         grepl("grains", fooditem, ignore.case = TRUE) 
       ,
       select = c(code, fooditem, scientific_name, WATER, ID_3))

subset(wafct, str_detect(fooditem, "Egg") & 
         grepl("", fooditem, ignore.case = TRUE),
       select = c(code, fooditem, ID_3, EDIBLE1, scientific_name))
subset(wafct, str_detect(scientific_name, "Scomberomorus"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name))

subset(dictionary.df,
       grepl("fig", FoodName_2, ignore.case = TRUE) &
         grepl("", FoodName_2, ignore.case = TRUE))

#Rename variables according to MAPS-standards

MAPS_output <- wafct %>%
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


MAPS_output %>% filter(!is.na(food_genus_id), is.na(food_subgroup))  

#Looking up foods in the fct

MAPS_output %>% filter(str_detect(original_food_name, "Corn")) %>% select(1:3) %>% knitr::kable()
MAPS_output %>% filter(str_detect(original_food_id, "120")) %>% select(1:2) %>% knitr::kable()
MAPS_output %>% filter(original_food_id == "02_042") %>% glimpse()
MAPS_output %>% filter(food_genus_id == "F0020.07")
  

#Checking for duplicated items
dim(MAPS_output)
which(duplicated(MAPS_output))

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_output$food_genus_id[!is.na(MAPS_output$food_genus_id)]))

#Checking that all dictionary codes have been matched to an entry in the dictionary
subset(MAPS_output, !is.na(food_genus_id) & is.na(food_genus_description))

#Saving file into csv to be used in MAPS tool
#v <- 6
#readr::write_excel_csv(MAPS_output,
#                      here::here('output',
#                          paste0('MAPS_WAFCT_v1.", v ".csv')) #that f(x) is to deal w/ special characters 
#       
