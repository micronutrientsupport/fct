
################################################################################
#                                                                              #
#                                                                              #                          
#           Kenya Food Composition Table (KENFCT, 2018)                        #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Library loading 

library(tidyverse)
library(visdat)

# 0) Accessing the data (for source of the data see README) - Uncomment!
# Only need to do it the first time to obtain the raw files!

#f <- "http://www.nutritionhealth.or.ke/wp-content/uploads/Downloads/Kenya%20Food%20Composition%20Tables%20Excel%20files%202018.xlsx"

#download.file(f, 
#             destfile = here::here("FCTs", 'KE18', "MOH-KENFCT_2018.xlsx"),
#             method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#            mode="wb")
#

# Data Import ----

KE18_Raw_FCT <- readxl::read_excel(here::here("FCTs", 'KE18', "MOH-KENFCT_2018.xlsx"), sheet = 4, skip = 2) %>% #reads the excel document and loads in the relevant sheet
  mutate(FCT = 'KE18') %>% #adding a column with the FCT short-name
  #mutate(nutrient_data_source = "None listed") %>% #adding a column making it clear no nutrient data source is listed in the table
  slice(1:1240) %>%   #removing last rows that are empty only provide notes info
  glimpse()



# Column Renaming ----

#Rename variables acc. to tagnames (FAO/INFOODS)
#We are not renaming Fatty acids nor AAs

ken_names <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'ENERC1', 'WATER', #creates a list of the new names
               'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG', 'ASH', 
               'CA', 'FE', 'MG', 'P', 'K', 'NA.', 'ZN', 'SE',
               'VITA_RAE', 'VITA', 'RETOL', 'CARTBEQ', 
               'THIA', 'RIBF', 'NIA', 'FOLDFE', 'FOLFD',
               'VITB12', 'VITC', 'CHOLE', 'OXALAC', 'PHYTCPPD', 'IP3', 'IP4',
               'IP5', 'IP6','FASAT', "FAMS","FAPU", 'FCT')

KE18_Raw_FCT <- KE18_Raw_FCT %>% rename_at(vars(1:37, 60:62, 320),  ~ken_names) #applies the list of new names

for (i in 1:319){ #This for loop checks to see if a cell in row 1 contains a brackets (i.e. is storing the column unit) and if so merges it with the column name
  first_row <- toString(KE18_Raw_FCT[1,i]) #creates a variable for the first row for the column being checked
  if(is.na(KE18_Raw_FCT[1,i])){ #checks if its NA - if it is, nothing happens
  } else if(startsWith(first_row, "(")){ #If it isn't NA and starts with '(', then proceeds with next steps
    first_row_tidied <- lapply(first_row, function(x) gsub("\\*|\\(|\\)", "", x)) #the contents of the first row are tidied to remove special characters
    colnames(KE18_Raw_FCT)[i] <- paste0(colnames(KE18_Raw_FCT)[i], first_row_tidied) #the cleaned first row entry is then merged with the column name
  } else{ #If the first row isn't NA and doesn't start with '(', then it is the FAO tagname and unit, but with special characters in between, and these actions occur:
    first_row_tidied <- lapply(first_row, function(x) gsub("\\*|\\(|\\)| ", "", x)) #The name is cleaned of special characters and then the column is renamed for this cleaned name
    colnames(KE18_Raw_FCT)[i] <- paste0(first_row_tidied) #and then the column is renamed for this cleaned name
  }
}

#This block checks the names are unique, and if they aren't then they are coerced into unique names
column_names <- colnames(KE18_Raw_FCT)
unique_column_names <- make.unique(column_names) #checks for unique names and coerces them to unique names if any are duplicates
colnames(KE18_Raw_FCT) <- unique_column_names



# creating variable 'foodgroups' ----

KE18_Raw_FCT_FoodGroup <- KE18_Raw_FCT %>%  filter(code %in% c(1:15)) %>% pull(fooditem) #This pulls the food group names by

KE18_Raw_FCT <- KE18_Raw_FCT %>% mutate(foodgroup = case_when( #This creates the foodgroup column, and assigns the values based on what the code starts with,
  str_detect(code, "[:digit:]{5}") & str_starts(code, '10') ~ KE18_Raw_FCT_FoodGroup[10], # which correlates directly with the food group
  str_starts(code, '10') ~ KE18_Raw_FCT_FoodGroup[1],
  str_starts(code, '20') ~ KE18_Raw_FCT_FoodGroup[2],
  str_starts(code, '30') ~ KE18_Raw_FCT_FoodGroup[3],
  str_starts(code, '40') ~ KE18_Raw_FCT_FoodGroup[4],
  str_starts(code, '50') ~ KE18_Raw_FCT_FoodGroup[5],
  str_starts(code, '60') ~ KE18_Raw_FCT_FoodGroup[6],
  str_starts(code, '70') ~ KE18_Raw_FCT_FoodGroup[7],
  str_starts(code, '80') ~ KE18_Raw_FCT_FoodGroup[8],
  str_starts(code, '90') ~ KE18_Raw_FCT_FoodGroup[9],
  str_starts(code, '11') ~ KE18_Raw_FCT_FoodGroup[11],
  str_starts(code, '12') ~ KE18_Raw_FCT_FoodGroup[12],
  str_starts(code, '13') ~ KE18_Raw_FCT_FoodGroup[13],
  str_starts(code, '14') ~ KE18_Raw_FCT_FoodGroup[14],
  str_starts(code, '15') ~ KE18_Raw_FCT_FoodGroup[15])) %>% 
  filter(!is.na(ENERC1kcal), !is.na(fooditem)) #Removing NA, SD/min-max



# creating low-quality value dataset ----

#Creating a dataset w/ the values that were of low quality [],  
#trace, fortified w/ folic acid or normal

ken_meta_quality <- KE18_Raw_FCT %>% #Subsets to a new dataframe, 'ken_meta_quality', when the quality indicators are included 
  mutate_at(vars(EDIBLE:F24D6g),  ~case_when(
    str_detect(. , '\\[.*?\\]') ~ "low_quality", #looks for low quality symbols; assigns them as low_quality
    str_detect(. , '[*]') ~ "folic-fortified", #looks for folic-fortified entries; denoted by square brackets
    str_detect(. , 'tr') ~ "trace", #looks for trace entries; denoted by "square brackets"tr"
    TRUE ~ "normal_value"))

#codes of the items identified as fortified with folic acid
folac <- KE18_Raw_FCT %>% filter(str_detect(FOLDFEmcg, '[*]')) %>% pull(code) 

#Extracting variables calculated with different (lower quality) method 
#and reported as using [] and removing * from FOLDFE
#and changing tr w/ 0

no_brackets_tr_ast <- function(i){ 
  case_when(
    str_detect(i, 'tr|[tr]') ~ "0", #replaces trace values with 0
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'), #removes special characters from values with them mixed in
    str_detect(i, '[*]')  ~ str_extract(i, "[:digit:]+"),
    TRUE ~ i)
}

KE18_Raw_FCT <- KE18_Raw_FCT %>% 
  mutate_at(vars(EDIBLE:F24D6g), no_brackets_tr_ast) #Applies that function

#Check that all tr, [] and * are removed 
#NOTE: tr will be found in non-numeric variables (i.e., fooditem)
KE18_Raw_FCT %>% str_which(.,"tr|[tr]|[*]|\\[.*?\\]")



# Adding the reference (biblioID) and Scientific name ----

KE18_Raw_FCT <- KE18_Raw_FCT %>% 
  left_join(., readxl::read_excel(here::here( "FCTs", 'KE18', "MOH-KENFCT_2018.xlsx"), #Attaches sheet 7 to the main table
                                  sheet = 7, skip = 2) %>%
              janitor::clean_names() %>% 
              select(2, 4,5) %>% #Selects the columns to merge
              mutate_at("code_kfct18", as.character),
            by = c("code" = "code_kfct18")) #merges them based on the code

KE18_Raw_FCT <- KE18_Raw_FCT[!duplicated(KE18_Raw_FCT), ] #Removes the duplicates that the last batch of code introduces for whatever reason

# Reordering variables and converting nutrient variables into numeric ----

KE18_Raw_FCT <- KE18_Raw_FCT %>% dplyr::relocate(c(scientific_name, foodgroup, biblio_id), #Relocating some columns to after food item
                                                 .after = fooditem) %>%
  dplyr::relocate(FCT, .before = code) %>%  #relocating "FCT" to after "code"
  mutate_at(vars(EDIBLE:F24D6g), as.numeric) #converting all values in the main body of the table to numeric

KE18_Raw_FCT %>% head() #Optional - used to check the first 6 rows, the column headers, and the cell types, to make sure they're correct



# Calculating FOLAC and checking folate fortification ----
#Optional - check for folate

KE18_Raw_FCT %>%  
  mutate(FOLACmcg = (FOLDFEmcg-FOLFDmcg)/1.7) %>% #calculates FOLAC from FOLDFE and FOLDFD
  select(code, FOLACmcg) %>%  #subsets to only include the code and FOLACmcg columns
  filter(!code %in% folac, FOLACmcg> 0) %>% #filters further if the code is in the folac list, and the FOLACmcg is over 0
  arrange(FOLACmcg) %>% #Arranges based on FOLAC
  knitr::kable()#Creates a table

# Adding Alcohol (only 3 alcoholic beverages, from the name in docu (page 136))
# (12007) Wine, Red (9.4 % alcohol),  (12008) Wine, White, Dry (10.3 % alcohol), 
# (12009) Wine, White, Sweet (10.2 % alcohol)
# ALC(%)*0.789(g/mL)/density(g/mL)

KE18_Raw_FCT$ALCg <- 0
KE18_Raw_FCT$ALCg[KE18_Raw_FCT$code == "12007"] <- (9.4*0.789/0.99)
KE18_Raw_FCT$ALCg[KE18_Raw_FCT$code == "12008"] <- (10.3*0.789/0.995)
KE18_Raw_FCT$ALCg[KE18_Raw_FCT$code == "12009"] <- (10.2*0.789/1.015)


# Renaming, Checking, and Selecting ----

colnames(KE18_Raw_FCT)[8] <- "ENERCkJ" #Renames the Energy columns
colnames(KE18_Raw_FCT)[9] <- "ENERCkcal"


# Fixing spelling and typos in original KE18 file

#Scientific names "issues"
#Eggplant (4017) scientific name is "Solalum melongena", instead of 
#"Solanum melongena"

#The scientific name of Coriander, leaves, fresh, raw is wrong!
KE18_Raw_FCT$scientific_name[KE18_Raw_FCT$code == "13011"] <- "Coriandrum sativum"

#The scientific name of coconut (3 entries) is wrong
KE18_Raw_FCT$scientific_name[KE18_Raw_FCT$code %in% c("10002", "10003", "10004")] <- "Cocos nucifera"

#The scientific name of Arrowroot, peeled, raw is wrong!
KE18_Raw_FCT$scientific_name[KE18_Raw_FCT$code == "2002"] <- "Maranta arundinacea"

# Adding scientific name for a NA 
KE18_Raw_FCT$scientific_name[KE18_Raw_FCT$code == "5030"] <- "Ananas comosus"

#There is a typo in "Roti"
KE18_Raw_FCT$fooditem[KE18_Raw_FCT$code == "15003"] <- "Roti (Indian Chapati)"


#Detecting FOLAC (folic acid used in fortified food)
#15065 - Fortified. Misreported in excel but reported in pdf
#15019 - It seems fortified, although it's not reported as such. 

KE18_Raw_FCT %>% head() #Optional - used to check the first 6 rows, the column headers, and the cell types, to make sure they're correct
KE18_Raw_FCT %>% glimpse() #Optional - used to check the column names, to check they're correct visually


KE18_Raw_FCT %>% dplyr::select(F20D5gstandardized:F22D6g) %>% #Optional - Visualise the number od missing and present values for a certain section of the table
  vis_miss()

KE18_Raw_FCT <- KE18_Raw_FCT %>% rename( #Renames a number of variables - e.g. "code" is renamed as "fdc_id"
  fdc_id = "code", 
  food_desc = "fooditem",
  food_group = "foodgroup",
  source_fct = "FCT", 
  nutrient_data_source = "biblio_id", 
  Edible_factor_in_FCT = "EDIBLE", 
  PROCNTg = "PROTCNTg", 
  NAmg = "NA.mg") %>% #selecting the variables of interest
  dplyr::select(source_fct:PHYTCPPDmg, TRPmg, FASATgstandardized:FATRNgstandardized,
                F20D5gstandardized, F22D6gstandardized, ALCg) 



# Data Output ----

glimpse(KE18_Raw_FCT) #Optional - check the data before saving 

write.csv(KE18_Raw_FCT, file = here::here("FCTs", "KE18_FCT_FAO_Tags.csv"), row.names = FALSE)  #Saves the newly-cleaned table to the Output folder 

#Run this to clean the environment
rm(list = ls())
