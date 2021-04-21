

library(tidyverse)
library(rvest)

###------------------LOADING FCT_QA SPREADSHEAT---------------#####

FCT_QA <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                        'MAPS', '02_working-files', 'FCT_QA',
                                        'FCT_QA.xlsx'), sheet = 2)

####---------------------Extracting info from FAO/INFOOD site--------------#######

#Harvesting Name of the Location

FCT <- read_html("http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/") %>%
  html_nodes('.bodytext') %>%
  html_text()

#Harvesting Name of the FCT

FCT1 <- read_html("http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/") %>%
  html_nodes('li') %>%
  html_text()

#Cleaning both chr list into a dataset :) 

Geo <- FCT[c(1:12,14:20,22:26)]

Names <-  FCT1[31:80]

Names <-  tibble(Names) 

#I can't use this code to add a new sheet due to some issues with Java

#xlsx::write.xlsx(Names, file = "FCT_QA.xlsx",
 #          sheetName = "FCTlist", append = TRUE)

#Instead create a new csv to be included in FCT_QA

#write_excel_csv(Names, here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                               #   'FCT_Names.csv'))


###########------------------###------------------------#############



###---------- b) Component Information (OLD VERSION) (DON'T RUN)-----------####

#Reading the excel sheet names

readxl::excel_sheets(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                'MAPS', '02_working-files', 'fct',
                                '2019_WAFCT.xlsx')) 


#Loading FCT - wa information on components

WAFCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                             'MAPS', '02_working-files', 'fct',
                                             '2019_WAFCT.xlsx'), sheet = 2) %>% 
                                              slice(2:n()) %>% glimpse()
  
 

#creating a table for all the component and its methods by INFOODS tagnames

master <- WAFCT_compo %>% select(3,4, 9) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Analytical/determination method/definition in English') %>%
  unite('tagname', c('tagname', 'Unit'), sep = '_') %>% 
  spread(key = 'tagname', value = 'method') 

#Attempt to standardize names of the components

names(method) <- str_replace_all(names(method), c(' ' = '_', '-' = ''))

names(method) <- str_remove_all(names(method), c('\\[]', '\\['))

method <- method %>% rename(CARTEBQ_mcg = 'CARTBEQ_or_CARTB]_mcg',
                            FAT_g = 'FAT_or_FATCE]_g',
                            FIBTG_g = 'FIBTG_or_FIBC]_g',
                            FOL_mcg = 'FOL_or_[FOLSUM]_mcg',
                            NIAEQ_mg = 'NIAEQ_or_[NIA]_mg',
                            PHYTCPP_mg = 'PHYTCPP_or_[PHYTCPPD]_or_[PHYTCPPI]_mg',
                            VITE_mg = 'VITE_or_TOCPHA]_mg')


#Reading the excel sheet names

readxl::excel_sheets(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                'MAPS', '01_raw-data', 'fct', 'ken',
                                '2018_KENFCT.xlsx'))


#Loading FCT information on components

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '01_raw-data', 'fct', 'ken',
                                           '2018_KENFCT.xlsx'), sheet = 3) %>% slice(21:57)

#creating a table for all the component and its methods by INFOODS tagnames

df <- FCT_compo %>% select(2,3, 6) %>% 
  rename(tagname = '...2', method = '...6') %>%
  unite('tagname', c('tagname', '...3'), sep = '_') %>% 
  spread(key = 'tagname', value = 'method') %>% rename(ENERC_kcal = 'ENERC_kJ, kcal')


master <- bind_rows(method, df)

#Loading FCT information on components uFish

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '02_working-files', 'fct',
                                           'uFish1.0.xlsx'), sheet = 3, skip = 2) %>% slice(1:37)

#creating a table for all the component and its methods by INFOODS tagnames

df <- FCT_compo %>% select(2,3, 8) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Comment') %>%
  unite('tagname', c('tagname', 'Unit'), sep = '_') %>% 
  spread(key = 'tagname', value = 'method')


master <- bind_rows(master, df)

#Loading FCT information on components uPulses

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '02_working-files', 'fct',
                                           'uPulses1.0.xlsx'), sheet = 3 , skip = 2) %>% slice(1:36)

df <- FCT_compo %>% select(2,3, 7) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Comment') %>%
  unite('tagname', c('tagname', 'Unit'), sep = '_') %>% 
  spread(key = 'tagname', value = 'method')

master <- bind_rows(master, df)

write_csv(master, here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
          'MAPS', '02_working-files', 'fct','methods_R.csv'))

#####----------------------------------END---------------------------#######

###---------- b) Component Information NEW VERSION -----------------####


# Creating the MASTER

#1. Load WAFCT_compo 

#Reading the excel sheet names

readxl::excel_sheets(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                'MAPS', '02_working-files', 'fct',
                                '2019_WAFCT.xlsx')) 


#Loading FCT - wa information on components

WAFCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                             'MAPS', '02_working-files', 'fct',
                                             '2019_WAFCT.xlsx'), sheet = 2) %>% 
  slice(2:n()) %>% glimpse()

#1. create a dataframe with tagnames and method (master)

master <- WAFCT_compo %>% select(3, 9) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Analytical/determination method/definition in English') %>% 
  pivot_wider(names_from = tagname, names_repair = "unique", values_from = method, 
        values_fn = list(method = list) )  %>% mutate(FCT = 'WAFCT')

#2. Unlist the method as character string

master$ENERC <-  sapply(master$ENERC, paste, collapse = " ")

master <- master %>% mutate_all(unlist)

#Creating the other dataset


#1. KENFCT

#Reading the excel sheet names

readxl::excel_sheets(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                'MAPS', '02_working-files', 'fct',
                                '2018_KENFCT.xlsx'))


#Loading FCT information on components

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '02_working-files', 'fct',
                                           '2018_KENFCT.xlsx'), sheet = 3) %>% slice(21:57)

FCT_compo[18,2] <- 'SE'

df <- FCT_compo %>% select(2, 6) %>% 
  rename(tagname = '...2',  method = '...6') %>%
  pivot_wider(names_from = tagname, names_repair = "unique", 
              values_from = method ) %>% mutate(FCT = 'KENFCT')

master <- bind_rows(master, df) %>% glimpse

#2. uFish

#Loading FCT information on components uFish

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '02_working-files', 'fct',
                                           '2016_uFish.xlsx'), sheet = 3, skip = 2) %>% 
  slice(1:36) %>% glimpse()

#creating a table for all the component and its methods by INFOODS tagnames

df <- FCT_compo %>% select(2, 8) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Comment') %>%
  pivot_wider(names_from = tagname, names_repair = "unique", 
              values_from = method ) %>% mutate(FCT = 'uFish')


master <- bind_rows(master, df)

#Loading FCT information on components uPulses

FCT_compo <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                           'MAPS', '02_working-files', 'fct',
                                           '2017_uPulses.xlsx'), sheet = 3 , skip = 2) %>% 
  slice(2:36) %>% glimpse()

df <- FCT_compo %>% select(2, 7) %>% 
  rename(tagname = 'INFOODS tagname', method = 'Comment') %>%
  pivot_wider(names_from = tagname, names_repair = "unique", 
              values_from = method ) %>% mutate(FCT = 'uPulses')

master <- bind_rows(master, df)


write_csv(master, here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                             'MAPS', '02_working-files', 'FCT_QA','methods_R.csv'))

#############----------------------END---------------------------######

###------------------- c) Quality Checks ----------------------####

#1) Standardization of variables names


##View WAFCT structure

readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                              'MAPS', '02_working-files',  'fct', 
                              '2019_WAFCT.xlsx'), sheet = 5) %>% head()


##Loading WAFCT, skip 2 first rows to use tagnames for components

WAFCT <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                       'MAPS', '02_working-files',  'fct', 
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

#Exrtacting variables names only in English

fgwa <- WAFCT %>% filter(is.na(fooditem), !is.na(code)) %>% pull(code) %>%
  str_split_fixed( '/', n = 2) %>% as.tibble() %>% pull(V1)


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


WAFCT<- WAFCT %>% mutate_at(vars(6:63), funs(as.numeric)) 

##View FCT structure
#Creating a variable for the names of the data-set 
# it is composed by 2 columns on the FCT_QA dataset
#Column 6 == Year
#Column 2 == Short_name
#Just need to input the number of the row (1-13)

x <- 2


readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                              'MAPS', '02_working-files',  'fct', 
                              paste(paste(FCT_QA[x,6],
                                  FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), sheet = 1) %>% head()


MAFOODS <- readxl::read_excel(here::here('OneDrive_London-School-of-Hygiene-and-Tropical-Medicine', 
                                       'MAPS', '02_working-files',  'fct', 
                                       paste(paste(FCT_QA[x,6],
                                  FCT_QA[x,2], sep = '_'), 'xlsx', sep = '.')), 
                              sheet = 1) %>% mutate(FCT = 'MAFOODS') %>% glimpse()

#Renaming variables with tagnames and converting numeric variables into numeric

FCT_tag <- c('code', 'ref', 'fooditem', 'foodgroup', 'WATER', 'ENERC1', 'ENERC2', 'NT',
              'PROTCNT', 'FAT', 'FASAT', 'FAMS', 'FAPU', 'CHOLC', 'CHOCSM', 'CHOAVLDF',
                'SUGAR', 'SUGAD', 'FIBC', 'STARCH', 'ASH', 'CA', 'FE', 'MG', 'P', 'K', 'NA',
             'ZN', 'CU', 'MN', 'ID', 'SE', 'VITA_RAE', 'VITA', 'THIA', 'RIBF', 'NIA', 'VITB6', 'FOL', 
             'VITB12', 'PANTAC', 'BIOT', 'VITC', 'VITD', 'VITE' ,'PHYT', 'FCT')


MAFOODS <- MAFOODS %>% rename_all( ~ FCT_tag) %>% mutate_at(vars(5:46), funs(as.numeric)) 


#Calculating missing values

FCT <- bind_rows(WAFCT, MAFOODS) %>% glimpse

FCT %>% filter(!is.na(fooditem)) %>% group_by (FCT) %>% summarise_all(funs(sum(is.na(.))))

#Trying some graphs

FCT %>% ggplot(aes(FCT, ZN)) + geom_boxplot() + facet_wrap(vars(foodgroup))




