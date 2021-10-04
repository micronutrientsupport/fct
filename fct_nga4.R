
#loading the packages

library(tidyverse)

## loading data

read.csv(here::here("data", "nga4_foodlist_raw.csv")) %>% 
separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
  mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+")) %>% 
  relocate(nga4_foodid, .before = nga4_fooditem)


nga4.foodlist <- read.csv(here::here("data", "nga4_foodlist_raw.csv")) %>% 
  separate_rows(nga4_fooditem, sep = "(?<=\\d)[[:blank:]]") %>% 
  mutate(nga4_foodid = str_extract(nga4_fooditem, "\\d+"),
         nga4_fooditem = str_remove(nga4_fooditem, "\\d+")) %>% 
  mutate_at("nga4_fooditem", str_squish) %>% 
  relocate(nga4_foodid, .before = nga4_fooditem)

## add the food genus code to each item


## add the WAFCT id


## check values for Zn, Fe
