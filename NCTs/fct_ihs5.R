
#loading the packages

library(tidyverse)

## loading data
ihs5 <- read.csv("ihs5-fct_v1.2.csv") %>% select(-X)

AHHA <- read.csv(here::here("data", "AHHA-FCT.csv"))

source("dictionary.R")

source("wafct.R")

source("mafood.R")

#Cleaning variables


#ref_item is empty and fooditem is ref_item

#Generating a FCT for IHS5 to be use w/ ihs5 code
#THIS IS NOT TO BE USED W/I MAPS TOOL!

#Removing GeNUS and creating unique items
fct_ihs5 <- ihs5 %>% select(-food_genus_id) %>% distinct() 

#Cleaning variables

#ref_item is empty and fooditem is similar to ref_fctitem
#we are going to combine them

identical(fct_ihs5$ref_fctitem, fct_ihs5$fooditem)

fct_ihs5 %>% select(ihs5_foodid, ref_fctitem, fooditem)

fct_ihs5 <- fct_ihs5 %>%
  select(-ref_item) %>% 
  mutate(
  ref_fctitem = ifelse(!str_detect(ref_fctitem, "\\b")| is.na(ref_fctitem),
                      fooditem, ref_fctitem)) %>% 
  select(-fooditem) 

##Adding item 118

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid == "103") %>% 
  mutate(ihs5_foodid = 118) %>% 
  bind_rows(., fct_ihs5)

#Changing 412 to MW02_0003

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != 412) %>% 
  bind_rows(., mwi_clean %>% filter(code == "MW02_0003") %>% 
  mutate(ihs5_foodid = 412,
         ihs5_fooditem = "Tinned vegetables",
         ref_source = "MAFOODS") %>% 
  rename(ref_fctcode = "code",
         ref_fctitem = "fooditem"))  %>% 
  select(ihs5_foodid:ID_cal) 

#Changing 829 to combo meal
#MISSING retention factors



rf_var <- c('fooditem', 'VITA_RAE', 'VITB1', "VITB2", "NIA", 
            "FOLDFE", "VITB12", "VITC", "CA", "FE", "MG", "P",
            "K", "NA.", "ZN", "SE", "PHYTCPPD")

            

rf_leaf <- readxl::read_excel(here::here('data', "MOH-KENFCT_2018.xlsx"), sheet = 6) %>% 
  janitor::clean_names() %>% 
  rename(item = "retention_factors_applied_according_to_the_cooking_methods_deriving_from_vasquez_caicedo_2008") %>% 
  filter(item %in% c(NA,  "Leafy vegetables, boiled")) 

rf_leaf <- rf_leaf %>% slice(2:3) %>% 
  bind_cols(., rf_leaf %>% slice(5:6))  %>%
  select(-c(x9...9,x10...10, item...11)) %>%  
  rename_all(., ~rf_var) %>% 
  mutate(ref_fct = "KENFCT") %>% slice(2) %>% 
  mutate_at(c('VITA_RAE', 'VITB1', "VITB2", "NIA", 
              "FOLDFE", "VITB12", "VITC", "CA", "FE", "MG", "P",
              "K", "NA.", "ZN", "SE", "PHYTCPPD"), as.numeric)


meal_vendor <- c("MW01_0031", "MW03_0010", "MW04_0020")

item_name <- fct_ihs5 %>% filter(ihs5_foodid == 829) %>% pull(ihs5_fooditem)

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != 829) %>% 
  bind_rows(., 
        mwi_clean %>% filter(code %in% meal_vendor) %>% 
     mutate(ihs5_foodid = 829) %>% 
  group_by(ihs5_foodid) %>% 
    summarise_if(is.numeric, mean, na.rm =T) %>% 
  left_join(., mwi_clean %>% filter(code %in% meal_vendor) %>% 
  mutate(ihs5_foodid = 829) %>% 
  group_by(ihs5_foodid) %>% 
    summarise_if(is.character, ~paste(., collapse = ", ")) %>% 
    rename(ref_fctcode = "code",
           ref_fctitem = "fooditem")) %>% 
    mutate(ihs5_fooditem = item_name,
           ref_source = "MAFOODS" )) %>% 
  select(ihs5_foodid:ID_cal)  

#Fixing OFSP and WFSP

sp_id <- c("832", "831", "204", "203")

fct_ihs5 %>% filter(ihs5_foodid %in% sp_id) %>% arrange(ihs5_foodid)

fct_ihs5 <- fct_ihs5 %>% mutate_at("ihs5_foodid", as.character) %>% 
  bind_rows(., fct_ihs5 %>% filter(ihs5_foodid == "203") %>%  
  mutate(ihs5_foodid = "204b",
         ihs5_fooditem = "Orange sweet potato",
         VITA_RAE = 925.830, 
         comment = "OSF equal compo as 203 (MW01_0065) 
         but VITA_RAE from (MW01_0063)")) %>% 

  bind_rows(.,fct_ihs5 %>% filter(ihs5_foodid == "832") %>%  
  mutate(ihs5_foodid = "832b",
         ihs5_fooditem = "Roasted orange sweet potatoes",
         VITA_RAE = 925.830*0.83, 
         comment = "OSF equal compo as 832 (AHHA - 5010) 
         but VITA_RAE from (MW01_0063*retention factor 
         calculated from Carpio et al., 2017)")) %>% 

  bind_rows(., fct_ihs5 %>% filter(ihs5_foodid == "831") %>%  
  mutate(ihs5_foodid = "831b",
         ihs5_fooditem = "Boiled orange sweet potatoes",
         VITA_RAE = 925.830*0.95, 
         comment = "OSF equal compo as 831 (MAFOODS - MW01_0066) but VITA_RAE
         from (MW01_0063*retention factor KENFCT - Starchy root or potato,
         boiled)")) 

# Changed VITA_RAE of white boiled sweet potato to match raw white sweet potato
#Difference in water is <10% there is no need to water adjust 
#2.213000*(100-76.00/100-77.28)

fct_ihs5$VITA_RAE[fct_ihs5$ihs5_foodid == "831"] <-  2.213000*0.95
fct_ihs5$comment[fct_ihs5$ihs5_foodid == "831"] <- "VITA_RAE from (MW01_0063*retention factor KENFCT - Starchy root or potato,boiled)"


#Adding the infant formula compo #708

formula_id <-  mwi_clean %>% filter(str_detect(code, "MW07"), 
                     str_detect(fooditem, ("powder")), 
                     !str_detect(fooditem, "follow-up")) %>% pull(code)

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != "708") %>% 
  bind_rows(., 
            mwi_clean %>% filter(code %in% formula_id) %>% 
              mutate(ihs5_foodid = "708") %>% 
              group_by(ihs5_foodid) %>% 
              summarise_if(is.numeric, mean, na.rm = T) %>% 
              left_join(., mwi_clean %>% filter(code %in% formula_id) %>% 
                          mutate(ihs5_foodid = "708") %>% 
                          group_by(ihs5_foodid) %>% 
                          summarise_if(is.character, ~paste(., collapse = ", ")) %>% 
                          rename(ref_fctcode = "code",
                                 ref_fctitem = "fooditem")) %>% 
              mutate(ihs5_fooditem = "Infant feeding formula (for bottle)",
                     ref_source = "MAFOODS" , 
                     comment = "We are assuming that VITA is coming
                                   from as retinol and VITA = VITA_RAE" )) %>% 
  select(ihs5_foodid:comment)  

fct_ihs5$VITA_RAE[fct_ihs5$ihs5_foodid == "708"] <- 640.10


fct_ihs5 %>% write.csv(here::here("output", "fct_ihs5_v2.2.csv"), 
                       row.names = F)


read.csv(here::here("output", "fct_ihs5_v2.0.csv")) %>%
  count(ihs5_foodid) %>% arrange(desc(n))

read.csv(here::here("output", "fct_ihs5_v2.0.csv")) %>% 
  filter(ihs5_foodid == "412")

nct <- read.csv(here::here("output", "fct_ihs5_v2.2.csv")) 

names(nct)

nct %>% select(ihs5_foodid,  ref_fctcode) %>% 
  separate_rows( ref_fctcode,sep =";|,") %>%  
  mutate(ref_fctcode = str_squish(ref_fctcode)) %>% 
  left_join(., dict_comp, by = c("ref_fctcode" = "fdc_id") )  %>% View()
