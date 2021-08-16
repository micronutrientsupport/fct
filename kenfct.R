

library(tidyverse)


##0) DOWNLOADING KENYA FCT FROM HUMAN NUTRION AND DIETETICS UNIT, MOH, KENYA

#Only need to do it once!

f <- "http://www.nutritionhealth.or.ke/wp-content/uploads/Downloads/Kenya%20Food%20Composition%20Tables%20Excel%20files%202018.xlsx"

download.file(f,"./data/MOH-KENFCT_2018.xlsx",
              method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
              mode="wb")


##1) LOADING KENYA FCT 

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


#Extracting variables calculated with different (lower quality) method 
#and reported as using [] and removing them from the original variable

#Use function to remove bracket and then the FOLDFE
#Then convert to numeric to calculate FOLAC



#Detecting FOLAC (folic acid used in fortified food)

#MISSING 2 (17 vs 19)
#*(15065) includes 52 mcg of FOLAC

kenfct %>% filter(str_detect(FOLDFE, '[*]')) %>% pull(code, FOLDFE) 



kenfct %>% select(-fooditem) %>%  
  str_subset(., "[*]") 

kenfct %>%  
  mutate(FOLAC = ifelse( 
    str_detect(FOLDFE, '[*]'), str_extract(FOLDFE, "[:digit:]+"), FOLDFE)) %>% 
  filter(!is.na(FOLAC)) %>% count(FOLAC)

  mutate_if(is.character, no_brackets)
