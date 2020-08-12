
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
              'PROTCNT', 'FAT',  'CHOAVLM ', 'FIBTS', 'PHYTAC',
              'WATER', 'CA',  'P', 'FE',  'ZN', 'CARTBEQ', 'VITC',
              'ref', 'FCT')

#Renaming and converting into numeric (needed for binding datasets)

GMBFCT <- GMBFCT %>% rename_all( ~ FCT4_tag) %>% slice(3:527) %>%
  mutate_at(vars(4:17), funs(as.numeric)) 

#Converting into character (needed for binding datasets)

GMBFCT$code <- as.character(GMBFCT$code)

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

GMBFCT <- readxl::read_excel(here::here(  'data', 
                                        paste(paste(FCT_QA[x,6],
                                                    FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                             sheet = 1) %>% mutate(FCT = 'GMBFCT') %>% glimpse()

