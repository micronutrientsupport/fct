
library(tidyverse)
library(rvest)

###-------------------------LOADING FCT_QA SPREADSHEAT----------------------#####

FCT_QA <- readxl::read_excel(here::here('data', 'FCT_QA.xlsx'), sheet = 2)

######----------------------------------------------------------###################


###########====== USING TAGNAMES FAO/INFOODS for HARMONIZATION OF VARIABLES=====##############


##################------1) West Africa FCT----#####################


##View WAFCT structure

readxl::read_excel(here::here('data', 
                              '2019_WAFCT.xlsx'), sheet = 5) %>% head()


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
                                                               ifelse(grepl("07_", code),  fgwa[7],
                                                                      ifelse(grepl("08_", code), fgwa[8],
                                                                             ifelse(grepl("09_", code), fgwa[9], 
                                                                                    ifelse(grepl("10_", code), fgwa[10], 
                                                                                           ifelse(grepl("11_", code), fgwa[11],
                                                                                                  ifelse(grepl("12_", code),  fgwa[12],     
                                                                                                         ifelse(grepl("13_", code), fgwa[13],
                                                                                                                ifelse(grepl("14_", code), fgwa[14],
                                                                                                                       'NA'))))))))))))))) %>% 
  filter(!is.na(fooditem))

#Extracting variables calculated with different method and reported as using []


#This keeps [] 

#WAFCT <- WAFCT %>% mutate(FIBC = str_extract(FIBTG, "\\[.*?\\]"))


#This keeps only numbers

WAFCT <- WAFCT %>% mutate(FIBC = str_extract(FIBTG, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FATCE = str_extract(FAT, '(?<=\\[).*?(?=\\])')) %>%
  mutate(TOCPHA = str_extract(VITE, '(?<=\\[).*?(?=\\])')) %>%
  mutate(FOLSUM = str_extract(FOL, '(?<=\\[).*?(?=\\])')) %>%
  mutate(PHYTCPPD_I = str_extract(PHYTCPP, '(?<=\\[).*?(?=\\])'))

#Reordering variables

WAFCT <- WAFCT %>% dplyr::relocate(foodgroup, .after = ref) %>%
  dplyr::relocate(FCT, .before = code)

#WE don't need this bit anymore
#WAFCT <- WAFCT[,c(64, 1:4, 65, 5:69)]


#Converting into numeric numeric variables  

WAFCT<- WAFCT %>% mutate_at(vars(8:69), funs(as.numeric)) 

#Calculating SOP for the WAFCT


WAFCT <- WAFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`))


##################------2) Malawi FCT----#####################


##View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 2


readxl::read_excel(here::here( 'data', 
                              paste(paste(FCT_QA[x,6],
                                          FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>% head()


MAFOODS <- readxl::read_excel(here::here( 'data', 
                                         paste(paste(FCT_QA[x,6],
                                                     FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                              sheet = 1) %>% mutate(FCT = 'MAFOODS') %>% glimpse()

#Renaming variables with tagnames and converting numeric variables into numeric

FCT_tag <- c('code', 'ref', 'fooditem', 'foodgroup', 'WATER', 'ENERC1', 'ENERC2', 'NT',
             'PROTCNT', 'FAT', 'FASAT', 'FAMS', 'FAPU', 'CHOLC', 'CHOTSM', 'CHOAVLDF',
             'SUGAR', 'SUGAD', 'FIBC', 'STARCH', 'ASH', 'CA', 'FE', 'MG', 'P', 'K', 'NA',
             'ZN', 'CU', 'MN', 'ID', 'SE', 'VITA_RAE', 'VITA', 'THIA', 'RIBF', 'NIA', 'VITB6', 'FOL', 
             'VITB12', 'PANTAC', 'BIOT', 'VITC', 'VITD', 'VITE' ,'PHYT', 'FCT')


MAFOODS <- MAFOODS %>% rename_all( ~ FCT_tag) %>% mutate_at(vars(5:46), funs(as.numeric)) 

#Calculating SOP - Removing ALC and changing FIBTG for FIBC

MAFOODS <- MAFOODS %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBC',  'ASH'), `+`))



##################------3) Ethiopia FCT----#####################


#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 3


readxl::read_excel(here::here( 'data', 
                              paste(paste(FCT_QA[x,6],
                                          FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>%
  head()

#Customized saving FCT

ETHFCT <- readxl::read_excel(here::here('data', 
                                        paste(paste(FCT_QA[x,6],
                                                    FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 1, skip = 1) %>% mutate(FCT = 'ETHFCT') %>%
  select(1:21, 24) %>% glimpse()

#Renaming variables with tagnames and converting numeric variables into numeric

FCT3_tag <- c('code', 'fooditem', 'fooditemETH', 'ENERC1', 'WATER',  'NT',
              'PROTCNT', 'FAT', 'CHOT', 'FIBTG', 'ASH', 'CA', 'P', 'FE', 'CARTBEQ', 
              'THIA', 'RIBF', 'NIA', 'TRP', 'VITC', 'REFUSE', 'FCT')


ETHFCT <- ETHFCT %>% rename_all( ~ FCT3_tag)

#creating variable 'foodgroups'

#Extracting food groups names

fgeth <- ETHFCT %>% filter(is.na(code), !is.na(fooditem)) %>% pull(fooditem) 

#Extracting the position of food groups names

roweth <- which(is.na(ETHFCT$code) & !is.na(ETHFCT$fooditem))

#creating a table with both names and position

fgtable <- tibble(fgeth, roweth) 

#cleaning some names that were splitted into two rows

fgtable[16,1] <- 'NON-ALCOHOLIC BEVERAGES (home made)'
fgtable[18,1] <- 'ALCOHOLIC BEVERAGES (home produced)'
fgtable[20,1] <- 'ALCOHOLIC BEVERAGES (commercial, local)'
fgtable[22,1] <- 'ALCOHOLIC BEVERAGES (imported)'

fgtable <- fgtable %>%  slice(1, 4:16, 18,20, 22,24:26)

ETHFCT <- ETHFCT %>% slice(2:903) %>%
  mutate_at(vars(4:21), funs(as.numeric)) 

ETHFCT <- ETHFCT %>%
  mutate(SOP = reduce(select(.,       #We don't include fibre cause it's included in CHOT
                             'WATER', 'PROTCNT' ,'FAT', 'CHOT',  'ASH'), `+`))

###Waiting for the food group standardization after finishing with tagnames

##################------4) Gambia FCT----#####################

#Gambia FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 4


readxl::read_excel(here::here( 'data', 
                              paste(paste(FCT_QA[x,6],
                                          FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>%
  head()

#Customized saving FCT

GMBFCT <- readxl::read_excel(here::here( 'data', 
                                        paste(paste(FCT_QA[x,6],
                                                    FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 1) %>% mutate(FCT = 'GMBFCT') %>% glimpse()


FCT4_tag <- c('foodgroup','code',  'fooditem', 'ENERC1', 'ENERC2',  
              'PROTCNT', 'FAT',  'CHOAVLM', 'FIBTS', 'PHYTAC',
              'WATER', 'CA',  'P', 'FE',  'ZN', 'CARTBEQ', 'VITC',
              'ref', 'FCT')

#Renaming and converting into numeric (needed for binding datasets)

GMBFCT <- GMBFCT %>% rename_all( ~ FCT4_tag) %>% slice(3:527) %>%
  mutate_at(vars(4:17), funs(as.numeric)) 

#Converting into character (needed for binding datasets)

GMBFCT$code <- as.character(GMBFCT$code)

#SOP - changing CHOAVLDF to CHOAVLM, FIBTG to FIBTS, removing ALC, no ASH are included.

GMBFCT <- GMBFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLM','FIBTS'), `+`))

##################------5) Kenya FCT----#####################

#Kenya FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 5


readxl::read_excel(here::here(  'data', 
                              paste(paste(FCT_QA[x,6],
                                          FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 4) %>%
  head()

#Customized saving FCT

KENFCT1 <- readxl::read_excel(here::here(  'data', 
                                        paste(paste(FCT_QA[x,6],
                                                    FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 4)  %>% select(1:37) %>%  mutate(FCT = 'KENFCT') %>%
                                slice(1:1241) %>% glimpse()


FCT5_tag <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'ENERC1', 'WATER', 
            'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG', 'ASH', 
             'CA', 'FE', 'MG', 'P', 'K', 'NA', 'ZN', 'SE',
               'VITA_RAE', 'VITA', 'RETOL', 'CARBEQ', 
             'THIA', 'RIBF', 'NIA', 'FOLDFE', 'FOLDF',
             'VITB12', 'VITC', 'CHOLE', 'OXALAC', 'PHYTCPPD', 'IP3', 'IP4',
              'IP5', 'IP6', 'FCT')

KENFCT1 <- KENFCT1 %>% rename_all( ~ FCT5_tag) 

KENFCT1 <- KENFCT1 %>% slice(2:1241) %>%
  mutate_at(vars(3:37), funs(as.numeric)) 

KENFCT1 <- KENFCT1 %>%
  mutate(SOP = reduce(select(.,       #We don't include fibre cause it's included in CHOT
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF', 'FIBTG', 'ASH'), `+`))


KENFCT2 <- readxl::read_excel(here::here(  'data', 
                                           paste(paste(FCT_QA[x,6],
                                                       FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                              sheet = 4, skip = 3) %>% select(38:319) %>% glimpse()

KENFCT <- bind_cols(KENFCT1, KENFCT2)


##################------6) Lesotho FCT----#####################

#Lesotho FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 6


readxl::read_excel(here::here(  'data', 
                                paste(paste(FCT_QA[x,6],
                                            FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 6) %>%
  head()

#Customized saving FCT

LSOFCT <- readxl::read_excel(here::here(  'data', 
                                           paste(paste(FCT_QA[x,6],
                                                       FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
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

LSOFCT <- LSOFCT %>%   mutate_at(vars(3:39), funs(as.numeric)) 


LSOFCT <- LSOFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG',  'ASH'), `+`))


##################------8) Nigeria FCT----#####################

#Nigeria FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 8


readxl::read_excel(here::here(  'data', 
                                paste(paste(FCT_QA[x,6],
                                            FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>%
  head()


#Customized saving FCT

NGAFCT <- readxl::read_excel(here::here(  'data', 
                                          paste(paste(FCT_QA[x,6],
                                                      FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 1) %>% mutate(FCT = 'NGAFCT')  %>% glimpse()

NGAFCT <- NGAFCT %>% rename(
  ref = 'REFID',
  code = 'Code', 
  foodgroup = 'Category',
  fooditemNGA = 'LocalName',
  fooditem = 'EnglishName',
  scientificName = 'ScientificName',
  fooditemFR = 'FrenchNames',
  ENERC1 = 'ENERC_kcal',
  ENERC2 = 'ENERC_kJ')


#Removing units from variable names

names(NGAFCT) <- sub("_g|_mcg|_mg", "", names(NGAFCT))

NGAFCT <- NGAFCT %>% rename_at(vars(16:25), funs(toupper)) %>%
  rename(VITA_RAE = 'VIT_A_RAE',
         VITB6 = 'VIT_B6')


NGAFCT <- NGAFCT %>% mutate_at(vars(9:37), funs(as.numeric)) 


NGAFCT <- NGAFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FATCE', 'CHOCDF','FIB',  'ASH'), `+`))



##################------11) Uganda FCT----#####################

#Uganda FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 11


readxl::read_excel(here::here(  'data', 
                                paste(paste(FCT_QA[x,6],
                                            FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>%
  head()


#Customized saving FCT

UGAFCT1 <- readxl::read_excel(here::here(  'data', 
                                          paste(paste(FCT_QA[x,6],
                                                      FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                                   sheet = 1)  %>% select(1:13) %>% glimpse()


UGAFCT1 <- UGAFCT1 %>% rename(
                      code = 'food_code', 
                      fooditem = 'food_description',
                      ref = 'fct_source_descr',
                      foodgroup = 'food_group')

UGAFCT2 <- readxl::read_excel(here::here(  'data', 
                                          paste(paste(FCT_QA[x,6],
                                                      FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 1)   %>% select(14:39) %>% mutate(FCT = 'UGAFCT') %>% glimpse()


FCT11_tag <- c( 'WATER', 'DM', 'ENERC1', 
              'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG',  
              'CA', 'FE',  'ZN', 'VITC', 'THIA', 'RIBF',
              'NIA', 'VITB6', 'FOL', 'FOLAC', 'FOLDF', 'FOLDFE', 
              'VITB12', 'VITA_IU', 'VITA_RAE', 'RETOL', 'CARTA', 'CARTB' , 'CRYPXB', 
              'FCT')

UGAFCT2 <- UGAFCT2 %>% rename_all( ~ FCT11_tag) 


UGAFCT <- bind_cols(UGAFCT1, UGAFCT2)

UGAFCT <- UGAFCT %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG'), `+`))

UGAFCT$code <- as.character(UGAFCT$code)


##################------12) uFish FCT----#####################

#uFish FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 12


readxl::read_excel(here::here(  'data', 
                                paste(paste(FCT_QA[x,6],
                                            FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 4) %>%
  head()

#Customized saving FCT

uFish <- readxl::read_excel(here::here(  'data', 
                                           paste(paste(FCT_QA[x,6],
                                                       FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                              sheet = 4) %>% slice(2:516) %>% mutate(FCT = 'uFish')   %>% glimpse()

uFish <- uFish %>% rename(
  code = 'Food Item ID', 
  three_alpha = '3-Alpha',
  fooditem = 'Food name in English',
  food_state = 'State of food',
  ref = 'RefID',
  EDIBLE1 = 'EDIBLE...8',
  EDIBLE2 = 'EDIBLE...9',
  ENERC1 = 'ENERC(kcal)',
  ENERC2 = 'ENERC(kJ)')

names(uFish) <- sub("\\(.*?\\)", "", names(uFish))


uFish <- uFish %>% mutate_at(vars(8:168), funs(as.numeric)) 


uFish <- uFish %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG',  'ASH'), `+`))


##################------13) uPulses FCT----#####################

#uPulses FCT - tagname standardization

#View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 13


readxl::read_excel(here::here(  'data', 
                                paste(paste(FCT_QA[x,6],
                                            FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 4) %>%
  head()

#Customized saving FCT

uPulses <- readxl::read_excel(here::here(  'data', 
                                         paste(paste(FCT_QA[x,6],
                                                     FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                            sheet = 4) %>%  slice(2:177) %>% mutate(FCT = 'uPulses')   %>% glimpse()

uPulses <- uPulses %>% rename(
  code = 'FoodID', 
  origin = 'Country, region',
  fooditem = 'Food name in English',
  food_state = 'Processing',
  scientificName = 'Species/Subspecies',
  variety = 'Cultivar/Variety/Accession Name',
  ref = 'BiblioID',
  ENERC1 = 'ENERC(kcal)',
  ENERC2 = 'ENERC(kJ)')


names(uPulses) <- sub("\\(.*?\\)", "", names(uPulses))

uPulses <- uPulses %>% mutate_at(vars(8:68), funs(as.numeric)) 


uPulses <- uPulses  %>%
  mutate(SOP = reduce(select(.,
                             'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBTG',  'ASH'), `+`))









FCT <- bind_rows(WAFCT, MAFOODS, ETHFCT, GMBFCT, KENFCT1, LSOFCT, UGAFCT, uFish, uPulses)

FCT %>% filter(!is.na(fooditem)) %>% group_by (FCT) %>% summarise_all(funs(sum(is.na(.))))

#Quality checks - Variability of SOP

FCT %>% ggplot(aes(FCT, SOP)) + geom_boxplot() 

FCT %>% filter(FCT != 'ETHFCT') %>% 
  ggplot(aes(FCT, SOP)) + geom_boxplot() 

#Variability of key MN (minerals) by FCT

FCT %>% ggplot(aes(FCT, ZN)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, SE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, FE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, ID)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, CA)) + geom_boxplot() 

#Variability of key MN (vitamins) by FCT

FCT %>% ggplot(aes(FCT, VITA_RAE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, VITB12)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, VITC)) + geom_boxplot() 


# % of missing values

naniar::vis_miss(FCT)

#heatmap of missing values per FCT

naniar::gg_miss_fct(FCT, fct = FCT)

#heatmap of missing values of the key MNs by FCT

FCT %>% select('FCT', 'VITA_RAE', 'VITA', 'CARTB', 'VITC', 'VITB12', 'FOL', 'FOLDFE','FIBTG' , 'FIBC',
               'FIBTS' ,'SE',  'ZN', 'ID', 'FE' , 'CA', 'PHYT', 'PHYTCPP', 'PHYTCPPD_I', 'PHYTAC', 'SOP') %>% 
  naniar::gg_miss_fct(fct = FCT)


FCT %>% select('FCT', 'SE',  'ZN', 'ID', 'FE' , 'CA') %>% 
  naniar::gg_miss_fct(fct = FCT)

