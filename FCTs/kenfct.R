
################################################################################
#
#                          
#           Kenya Food Composition Table (KENFCT, 2018)
#
#
#
################################################################################



##0) DOWNLOADING KENYA FCT FROM HUMAN NUTRION AND DIETETICS UNIT, MOH, KENYA

#Only need to do it once!

#f <- "http://www.nutritionhealth.or.ke/wp-content/uploads/Downloads/Kenya%20Food%20Composition%20Tables%20Excel%20files%202018.xlsx"

#download.file(f,"./data/MOH-KENFCT_2018.xlsx",
#             method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#            mode="wb")
#

#Documentation is in fct_cleaning.Rmd.

## 1) LOADING PACKAGES, DICTIONARY, AND KENYA FCT -----

library(tidyverse)
source("MAPS_Dictionary-Protocol.R")

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

dim(kenfct)

## 2) TIDYING KENYA FCT ----

#Rename variables acc. to tagnames (FAO/INFOODS)
#We are not renaming Fatty acids nor AAs

ken_names <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'ENERC1', 'WATER', 
              'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG', 'ASH', 
              'CA', 'FE', 'MG', 'P', 'K', 'NA.', 'ZN', 'SE',
              'VITA_RAE', 'VITA', 'RETOL', 'CARTBEQ', 
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

# Dealing with special characters

# METADATA: Creating a dataset w/ the values that were of low quality [],  
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

dim(kenfct)

#Adding the reference (biblioID) and Scientific name to kenfct
#There are two duplicated irems in the biblio file (code #1025, 4018).
kenfct <- kenfct %>% left_join(., 
              unique(readxl::read_excel(here::here('data',
                                          "MOH-KENFCT_2018.xlsx"), 
                   sheet = 7, skip = 2) %>%
                  janitor::clean_names() %>% 
                  select(2, 4,5)) %>% 
                  mutate_at("code_kfct18", as.character),
                  by = c("code" = "code_kfct18")) 

#Checking for duplicated
dim(kenfct)

  
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

##3) MAPS type format ----

wafct.genus <- read.csv(here::here('metadata', 'MAPS_WAFCT_standard-list.csv'))

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()


#Fixing spelling and typos in original KE18 file

#Scientific names "issues"
#Eggplant (4017) scientific name is "Solalum melongena", instead of 
#"Solanum melongena"

#The scientific name of Coriander, leaves, fresh, raw is wrong!
kenfct$scientific_name[kenfct$code == "13011"] <- "Coriandrum sativum"

#The scientific namr of coconut (3 entries) is wrong
kenfct$scientific_name[kenfct$code %in% c("10002", "10003", "10004")] <- "Cocos nucifera"

# Adding NA 
kenfct$scientific_name[kenfct$code == "5030"] <- "Ananas comosus"

#There is a typo in "Roti"
kenfct$fooditem[kenfct$code == "15003"] <- "Roti (Indian Chapati)"

## Adding Dictionary codes (GENuS) code 

ken_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "1004",   "F0022.03",    "m",
  "6001",   "22241.01.01", "h",
  "12004",  "F0665.01",    "m", 
  "12003",  "23912.02.01", "m", 
  "4016",   "1232.01"  ,   "m",
  "11001",  "2910.01"  ,   "h",
  "10010",  "1379.9.01" ,  "l", 
 # "13027",  "1699.02"   ,  "m", #salt, ionized 
  "15026",  "F0022.05"  ,  "h",
  "1031",   "23710.01"  ,  "m", 
  "13028",  "1699.03"   ,  "h",
  "11003",  "23520.01"  ,  "m",
  "12005",  "23914.02"  ,  "m",
  "13031",  "21399.01.01" ,"m",
  "12008",  "24212.02.01" ,"m",
  "6026",   "22230.01.01" ,"m",
   "10006", "1442.01", "h",
   "05009", "1491.02.01", "l",
  "1041", "1199.9.01", "m",
  "1045", "111.01", "m", 
  "3001", "1701.05", "m", 
  "3010", "1703.01", "m",
 "13023", "1253.02.03", "h",
  "2009", "1510.01", "m", 
  "10009", "142.01", "l",
  "13006", "1652.01", "m", 
  "13007", "1652.02", "m", 
  "7009", "21121.03", "h", 
  "8010", "1501.05", "m",
  "1007", "F0020.01", "m",
  "6008", "22241.02.01", "h", 
   "1034" ,  "23161.01.01", "m",
  "13017", "1699.07", "h", 
   "3019", "1709.9.01", "m", 
  "10014", "1444.01", "m", 
  "10015", "1445.01", "m",
  "4019", "1212.04", "m",
  "4022", "1214.01", "h",
   "13019", "1252.01", "m", 
  "5024", "1317.01", "m", 
  "4011", "1251.01", "m", 
  "2005", "1290.9.01", "m",
 "8002", "1505.07", "h",
 "4034", "1290.9.02", "m", 
 "2004", "1313.01", "m", 
 "4001", "1215.02", "m", 
 "1023", "1290.01.01", "h", 
 "1051", "1290.01.03", "m", 
 "13024", "1253.01.01", "h",
 "6007", "22120.02", "h",
 "13011", "1290.9.03", "h", 
 "7020", "21113.02.01", "h", #assumed w/o bones bc EP = 1
  "15125", "F0022.08", "h", 
 "13015", "1699.10", "h", 
 "4013", "1290.9.04", "h", 
 "1005", "F0020.02", "m", 
 "5012", "1319.01", "m", 
 "7019", "21115.01", "m",
 "10003", "1460.02", "m",
  "9011", "21700.02.02", "m", 
 "9001", "F1243.01", "m",
 "5028", "1342.01.01", "m",
 "4014", "1235.04", "m", 
 "4037", "21399.02.01", "m", 
 "1030", "23710.02", "h",
 "15081", "23914.03", "m",
 "6019", "22110.02.01", "h",
 "5025", "1319.02", "h",
 "8011", "1553.02", "h",
 "5031", "1346.01", "h",
 "4021", "1254.01", "h", 
 "4012", "1213.01", "m", 
 "5027", "1345.01", "m",
 "5034", "1354.01", "h", 
 "7025", "21184.02.01", "m",#No info on prep.
  "7022", "21184.01.01", "m", #No info on prep.
 "4004", "1213.02", "h", 
 "9002", "21691.02.01", "h",
 "8012", "1527.02", "h",
 "4008", "1231.01", "h",
 "4009", "1231.02", "h",
 "4010", "1231.03", "h", 
 "15019", "F0020.06", "h", 
 "15020", "F0020.04", "h", 
 "15130" ,"F0020.05", "h",
 "15003", "F0022.04", "m", 
 "15025", "F0022.07", "m",
 "7001" , "21111.01.03", "h",
 "7002" , "21111.02.01", "l", #All other are w/o bones, we assumed the same 
 "4003",  "1290.9.06", "h",
 "4029", "1290.9.07", "h",
 "4038", "1290.9.08", "h",
 "1036", "F0022.02", "h",
 "3002", "1243.01", "h",
 "1024", "23120.05.01", "h", 
 "1026", "23120.05.02", "h", 
 "6009", "22270.01", "h", 
 "6010", "22270.02", "h",
 "6011", "22270.03", "h",
 "6012", "22270.04", "h",
 "6013", "22270.05", "h",
 "1038", "23120.06.01", "h", 
 "1040", "23120.06.02", "h",
 "8035", "1533.02", "h", 
 "11002", "1802.02", "h",
 "3005", "1701.05", "h", 
 "10008", "1372.01", "l", 
 "10002", "1460.01", "h", 
 "9007", "21641.01.01", "m",
 "5002", "1341.02", "h",
 "50012", "1341.03", "h", 
 "5007", "1314.01", "h", 
 "5008", "1314.02", "h", 
 "9003", "1523.01", "l", #no fish specified, we assumed cod bc it's the most common
 "1044", "23110.01", "m", 
 "13003", "F1232.09", "h",
 "3008", "1709.9.02", "h" , 
 "3017", "1709.9.03", "h" ,
 "4025", "21397.01.01", "h",
 "4002", "1235.03", "h",
 "4024", "21393.01.01", "h", 
 "1001", "1199.9.02", "h",
 "7008",  "21122.01", "h", 
 "7026", "21170.01.03", "h",
 "1012", "F0022.09", "h", 
 "1013", "F0022.10", "h", 
 "1014", "F0022.11", "h", 
 "1015", "F0022.12", "h",
 "1016" ,"F0022.13", "h",
 "5013", "21439.02.01", "h", 
 "5016", "21439.04.01", "h", 
 "5026", "21439.9.03", "h", 
 "5035", "21439.01.01", "h",
 "5018", "1349.2.01", "h",
 "5039", "1346.02", "h",
 "7015", "21156.01", "h",
 "7018", "21155.01", "h",
 "5005", "21419.99.01", "h", 
 "8015", "1520.01" , "h",
 "8016", "1520.02" , "h",
 "8017", "1520.03" , "h",
 "8037", "1520.04" , "h",
 "8038", "1520.05" , "h",
 "8039", "1520.06" , "h",
 "8018", "1507.01" , "h",
 "8019", "1507.02" , "h",
 "8020", "1507.03" , "h",
 "8021", "1507.04" , "h",
 "8022", "1507.05" , "h",
 "8023", "1507.06" , "h",
 "8024", "1507.07" , "h",
 "8025", "1507.08" , "h",
 "8026", "1507.09" , "h",
 "8027", "1507.10" , "h",
 "8028", "1507.11" , "h",
 "8029", "1507.12" , "h",
 "8030", "1507.13" , "h",
 "8031", "1557.01", "h", 
 "8033", "1557.02", "h", 
 "8034", "1533.04" , "h",
 "8036",  "1533.05", "h", 
 "8040",  "1533.06", "h", 
 "8041",  "1533.07", "h", 
 "8042",  "1533.08", "h", 
 "15063", "F1232.10", "h", 
 "4035",  "1234.03", "h", 
 "2015",  "1530.01", "m", 
 "5020", "1316.05", "h", 
 "7014", "21116.02", "h",
 "1037", "114.02", "h", 
 "1011", "F0022.01", "m", 
 "4023", "1270.01", "h", 
 "7017", "21170.01.02", "h", 
 "10012", "21495.02.01", "h", 
 "8004", "1501.10", "h",
 "8022", "1507.05", "h",
 "1006", "F0020.07", "h",
 "1060", "23161.02.03", "h", 
 "5029", "21491.01", "m", 
 "10007", "21422.01", "h", 
 "10011", "1375.01", "m", # Check EP (as in this fct is shelled)
 "6015", "22222.02.01", "h", 
 "6016", "22222.01.01", "h", 
 "6003", "22251.01.02", "h", 
 "6004", "22251.01.03", "h", 
 "6005", "22251.01.04", "h",
 "14001", "21170.92.07", "h",
 "14002", "21170.92.08", "h",
 "14004", "21170.92.09", "h",
 "1029", "39120.06.01", "h",
 "15134" ,  "F0875.02", "h",
 "6020",  "22290.04", "m", 
 "6021",  "22290.05", "m",
 "5038", "1221.01", "m", 
 "4015", "1290.9.05", "h",
 "3015", "1242.01", "h", 
 "3012",  "1241.9.01", "h", 
 "13033", "F1232.16", "m" , 
 "1033", "23161.02.02", "h",
 "1003", "F0022.15", "h",
 "5001", "1341.03", "m",
 "5006", "1319.05", "m", 
 "5009", "1359.9.04", "h",
 "5011", "1316.02", "m",
 "5021", "1359.9.05", "h",
 "5032", "1359.9.06", "h", 
 "5033", "1353.01.01", "h",
 "5037" , "1359.9.07", "h", 
 "13020", "1699.11", "h", 
 "4006", "1212.05", "h", 
 "2012", "1290.9.11", "m", 
 "2010" ,"1290.9.13", "h", 
 "2011", "1290.9.14", "h"
 )



dictionary.df %>% filter(ID_2 == "113")

ken_genus <- read.csv(here::here("inter-output", "kenfct_matches.csv")) %>% 
  filter(!FCT.code %in% c("7009", "10010")) %>% #removing chicken - wrong code (21121.02) and macadamia wrong confidence
  select(FCT.code, MAPS.ID.code, Confidence) %>% 
  mutate(confidence = case_when(
    Confidence == "high" ~ "h", 
    Confidence == "medium" ~ "m", 
    Confidence == "low" ~ "l", 
    TRUE ~ Confidence)) %>% select(-Confidence) %>%
  mutate_at("FCT.code", as.character) %>% 
   rename(ref_fctcode = "FCT.code", 
         ID_3 = "MAPS.ID.code") %>% 
  bind_rows(ken_genus) %>% distinct()

(dupli <- ken_genus %>%  count(ref_fctcode) %>% 
  filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

ken_genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))
kenfct %>% filter(code %in% dupli) %>% arrange(desc(code)) %>% select(code, fooditem)

#Fixing horse bean to broad bean code (but they are all fava vicia)
ken_genus$ID_3[ken_genus$ref_fctcode == "3001"] <-  "1702.02"
#Fixing rice - acc. to SUA for Kenya all milled rice was coded
#23161.02 (whether imported or produced), hence we are changing
ken_genus$ID_3[ken_genus$ref_fctcode == "1034"] <-  "23161.02.01"
#Fixing beef to acc. for fat content variability
ken_genus$ID_3[ken_genus$ref_fctcode == "7004"] <-  "21111.01.02"
#Amend baking powder (1699.05) --> F1232.07 - Removing the code very high Ca (see documentation)
ken_genus$ID_3[ken_genus$ref_fctcode == "13002"] <-  NA #"F1232.07"
#Fixing samosa dictionary code
ken_genus$ID_3[ken_genus$ref_fctcode == "15025"] <-  "F0022.06"
#Millet, bulrush == Pearl millet (see docu)
ken_genus$ID_3[ken_genus$ref_fctcode == "1025"]  <- "118.03"
#Cabbage white updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "4007"]  <- "1212.03"
#Tomato, red updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "4036"]  <- "1234.02"
#Sweet potato, brown skin updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "2013"]  <- "1530.08"
#Mango ripe updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "5019"]  <- "1316.04"
#Goat medium fat updated dict code & confidence
ken_genus$ID_3[ken_genus$ref_fctcode == "7016"]  <- "21116.03"
ken_genus$confidence[ken_genus$ref_fctcode == "7016"]  <- "h"
#Sorghum, grain, white updated dict code & confidence
ken_genus$ID_3[ken_genus$ref_fctcode == "1039"]  <- "114.03"
ken_genus$confidence[ken_genus$ref_fctcode == "1039"]  <- "h"


#Updating the dictionary compilation -----
#for further use (to update versions)
v <- 1.2
ken_genus %>% mutate(fct = "KE18") %>% 
 write.csv(., here::here("metadata",
                        paste0("dict_fct_compilation_v",v, ".csv")), 
                    row.names = F)

kenfct <- kenfct %>% 
  left_join(., ken_genus, by = c("code" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = fooditem)

dim(kenfct)

#Checking dictionary/ fct ids availability ----

kenfct %>% filter(code %in% c("6003",
"6004",
"6005",
"6006")) %>% .[, c(3:4)]

subset(kenfct, code %in% c("13023"), 
       select = c(code, fooditem, ID_3, scientific_name))

subset(kenfct, code == "4006", select = c(fooditem, ID_3, scientific_name)) 
subset(kenfct, ID_3 == "F1232.02") 

dictionary.df %>% filter(ID_3 %in% c("21439.01"))
subset(dictionary.df, ID_2 == "1290.9")
subset(dictionary.df, ID_2 %in% c("1379.02"
                                      ))
subset(dictionary.df, ID_1 == "2602")
distinct(subset(dictionary.df, ID_0 == "CE"), select = FoodName_1)

subset(kenfct, grepl("radi", fooditem, ignore.case = TRUE) &
         grepl("", fooditem, ignore.case = TRUE),
       select = c(code, fooditem, scientific_name, WATER))
subset(kenfct, str_detect(code, "^5"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name)) %>% View()
subset(kenfct, str_detect(scientific_name, "triloba"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name))

subset(dictionary.df, grepl("radi", FoodName_3, ignore.case = T) &
              grepl("", FoodName_2, ignore.case = T))

subset(dictionary.df, grepl("cabba", scientific_name, ignore.case = T) &
         grepl("", FoodName_2, ignore.case = T))


#Rename variables according to MAPS-standards

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


MAPS_ken %>% head()

MAPS_ken %>% filter(str_detect(original_food_name, "lea")) %>% select(1:3) %>% knitr::kable()
MAPS_ken %>% filter(str_detect(original_food_id, "120")) %>% select(1:2) %>% knitr::kable()
MAPS_ken %>% filter(original_food_id == "9011") %>% glimpse()
MAPS_ken %>% filter(food_genus_id == "F0020.07")

#Checking for duplicated items
dim(MAPS_ken)
which(duplicated(MAPS_ken))

MAPS_ken[which(duplicated(MAPS_ken)),]
subset(MAPS_ken, original_food_id == "1025")

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"]))
x <- which(duplicated(MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"]))

((MAPS_ken$food_genus_id[MAPS_ken$food_genus_id != "NA"][x]))
subset(MAPS_ken, food_genus_id == "118.02")

#Checking that all dictionary codes have been matched to an entry in the dictionary

subset(MAPS_ken, !is.na(food_genus_id) & is.na(food_genus_description))

#Saving file into csv to be used in MAPS tool
#readr::write_excel_csv(MAPS_ken, here::here('output', 
 #                                 'MAPS_KENFCT_v1.7.csv'))
                          