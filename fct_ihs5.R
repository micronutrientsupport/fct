
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


#Changing 829 to combo meal


#Fixing OFSP and WFSP

