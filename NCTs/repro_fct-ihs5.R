
###############################################################################
#
#
#     Code to reproduce food composition and matches for the analysis of
#                 Malawi 2019-2020 IHS5 Survey 
#               National Statistical Office (NSO), 
#                 Government of Malawi
#    https://microdata.worldbank.org/index.php/catalog/3818/study-description
#                
#
#
#
#
###############################################################################

#Loading the data

#Food composition data

#1) fct used Tang et al., (2021)

fct_ihs4 <- read.csv(here::here("data", "old-files",  "food-match-mwi_v04.1.csv")) %>% 
  select(-c(X, ref_code, ref_fct, ref.x, ref.x.x, ref, comments))

#2) Malawian FCT (2019) 

#source("mafood.R")
source(here::here("FCTs", "mw19_dict.R"))

#List of unique fooditems in ihs5

ihs5_foodlist <- read.csv(here::here("inter-output","ihs5-fct_v1.2.csv")) %>% 
  select(-c(X ,food_genus_id)) %>% distinct() %>% .[, c(1,2)]


#Comparing food item list of ihs4 and ihs5 


ihs5_foodlist %>% mutate_at("ihs5_foodid", as.character) %>% 
  anti_join(., fct_ihs4,
            by = c("ihs5_foodid" = "code_ihs4",
                   "ihs5_fooditem" = "fooditem_ihs4"))

ihs5_foodlist %>% mutate_at("ihs5_foodid", as.character) %>% 
  left_join(., fct_ihs4,
            by = c("ihs5_foodid" = "code_ihs4",
                   "ihs5_fooditem" = "fooditem_ihs4")) %>% 
  filter(str_detect(ihs5_fooditem, "madeya"))

fct_ihs5 <- ihs5_foodlist %>% mutate_at("ihs5_foodid", as.character) %>% 
  left_join(., fct_ihs4,
            by = c("ihs5_foodid" = "code_ihs4",
                   "ihs5_fooditem" = "fooditem_ihs4"))


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

#Adding 118 - 'MAIZE UFA RAW MADEYA (bran flour - unprocessed)

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid == "103") %>% 
  mutate(ihs5_foodid = "118") %>% 
  bind_rows(., fct_ihs5)

#Adding 837 -  kalongonda (mucuna)



#Adding 835 - popcorn
fct_dict %>% filter(
  grepl("popcorn", food_desc, ignore.case = "TRUE"), 
  !grepl("chicken|industrial", food_desc, ignore.case = "TRUE")) %>% 
  select(source_fct, fdc_id, food_desc, ID_3,  WATERg, SEmcg)


#Changing 412 to MW02_0003

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != "412") %>% 
  bind_rows(., mwfct %>% filter(fdc_id == "MW02_0003") %>% 
              mutate(ihs5_foodid = "412",
                     ihs5_fooditem = "Tinned vegetables",
                     ref_source = "MW19") %>% 
              rename(ref_fctcode = "fdc_id",
                     ref_fctitem = "food_desc"))  %>% 
  select(ihs5_foodid:ZN) 

#Changing 829 to combo meal


meal_vendor <- c("MW01_0031", "MW03_0010", "MW04_0020")

item_name <- fct_ihs5 %>% filter(ihs5_foodid == "829") %>% pull(ihs5_fooditem)

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != "829") %>% 
  bind_rows(., 
            mwfct %>% filter(fdc_id %in% meal_vendor) %>% 
              mutate(ihs5_foodid = "829") %>% 
              group_by(ihs5_foodid) %>% 
              summarise_if(is.numeric, mean, na.rm =T) %>% 
              left_join(., mwfct %>% filter(fdc_id %in% meal_vendor) %>% 
                          mutate(ihs5_foodid = "829") %>% 
                          group_by(ihs5_foodid) %>% 
                          summarise_if(is.character, ~paste(., collapse = ", ")) %>% 
                          rename(ref_fctcode = "fdc_id",
                                 ref_fctitem = "food_desc")) %>% 
              mutate(ihs5_fooditem = item_name,
                     ref_source = "MW19" )) %>% 
  select(ihs5_foodid:ZN)  

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
                     comment = "OSF equal compo as 832 (AHHA - 5010) but VITA_RAE
         from (MW01_0063*retention factor KENFCT - Starchy root or potato,
         boiled)")) 

#Adding the infant formula compo #708

formula_id <-  mwfct %>% filter(str_detect(fdc_id, "MW07"), 
                                    str_detect(food_desc, ("powder")), 
                                    !str_detect(food_desc, "follow-up")) %>% pull(code)

fct_ihs5 <- fct_ihs5 %>% filter(ihs5_foodid != "708") %>% 
  bind_rows(., 
            mwfct %>% filter(fdc_id %in% formula_id) %>% 
              mutate(ihs5_foodid = "708") %>% 
              group_by(ihs5_foodid) %>% 
              summarise_if(is.numeric, mean, na.rm = T) %>% 
              left_join(., mwfct %>% filter(fdc_id %in% formula_id) %>% 
                          mutate(ihs5_foodid = "708") %>% 
                          group_by(ihs5_foodid) %>% 
                          summarise_if(is.character, ~paste(., collapse = ", ")) %>% 
                          rename(ref_fctcode = "fdc_id",
                                 ref_fctitem = "food_desc")) %>% 
              mutate(ihs5_fooditem = "Infant feeding formula (for bottle)",
                     ref_source = "MW19" , 
                     comment = "We are assuming that VITA is coming
                                   from as retinol and VITA = VITA_RAE")) %>% 
  select(ihs5_foodid:comment)  

fct_ihs5$VITA_RAE[fct_ihs5$ihs5_foodid == "708"] <- 640.10

#Creating a csv file for external use

#fct_ihs5 %>% write.csv(here::here("output", "fct_ihs5_v2.1.csv"), 
 #                      row.names = F)

