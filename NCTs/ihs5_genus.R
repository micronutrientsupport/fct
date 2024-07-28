

library(tidyverse)


c("food_genus_description"    , "food_group"               ,  "food_subgroup" ,
  "food_genus_confidence"    ,  "fct_name"  , "energy_in_kj", 
  "nitrogen_in_g"       ,       "totalprotein_in_g"      ,    "totalfats_in_g" ,
  "saturatedfa_in_g"           "monounsaturatedfa_in_g" ,    "polyunsaturatedfa_in_g" ,
  "cholesterol_in_mg"          "carbohydrates_in_g"      ,   "fibre_in_g"  ,
  "ash_in_g" , )

#popcorn - AHHA - 1009 


ihs5 <- read.csv(here::here("inter-output",  "ihs5-fct_v1.1.csv"))

names(ihs5)

subset(ihs5, ihs5_foodid == "812")

#mucuna - WAFCT - 03_059 and Joy et al., 
#unprocess -

 ihs5 %>% 
  relocate(food_genus_id, .after = "ihs5_fooditem") %>% 
  mutate(ref = ifelse(is.na(ref.x), ref.x.x, 
                      ifelse(is.na(ref), ref.x, ref))) %>% 
  select(-c("FoodName_3":"FoodName_2")) %>% 
  select(-ends_with("_code")) %>% 
   select(-ends_with("_fct")) %>% 
   select(-ends_with(".x")) %>% 
  filter(!food_genus_id %in% c("1530.06", "1321.02", "1322.02", "1324.02" )) %>% 
  mutate(food_genus_id = case_when(
    food_genus_id == "24230.03.03|24230.03.02" ~ "24230.03.02",
    food_genus_id == "1290.01.01|1290.01.02" ~ "1290.01.03",
    TRUE ~ food_genus_id)) %>% 
       count(ref_fctcode) %>% arrange(desc(n))
 
#We need to remove some items that are not well represented,
#those are food items with a genus code that did not correspond to
#a food compo entry the list is a follow:
# "1530.06" ==  to orange, sweet potato
# "1321.02" == pomelo/ grapefruit
# "1322.02" == limes,
# "1324.02" == mandarines
# 
# to be checked! 406 - other cultivated leafy
# 
# other issues is wrong coding at consumption level (i.e., applying different
 #genus_id to same compo item. that was the case for 24230.03.02 - maize beer), 
 #same happened w/ green maize
 
 #BEST TO CHECK ALL ITEMS AND MATCHES ONE BY ONE!!!

 # Infant formula is in the file name: food-match-mwi_v02.csv
 #

# #
# #
 
 
 ihs5 %>% 
   relocate(food_genus_id, .after = "ihs5_fooditem") %>% 
   mutate(ref = ifelse(is.na(ref.x), ref.x.x, 
                       ifelse(is.na(ref), ref.x, ref))) %>% 
   select(-c("FoodName_3":"FoodName_2")) %>% 
   select(-ends_with("_code")) %>% 
   select(-ends_with("_fct")) %>% 
   select(-ends_with(".x")) %>% 
   filter(!food_genus_id %in% c("1530.06", "1321.02", "1322.02", "1324.02" )) %>% 
   mutate(food_genus_id = case_when(
     food_genus_id == "24230.03.03|24230.03.02" ~ "24230.03.02",
     food_genus_id == "1290.01.01|1290.01.02" ~ "1290.01.03",
     TRUE ~ food_genus_id)) %>% 
   filter(ih5_itemid != )
   write.csv(.,here::here("ihs5-fct_v1.2.csv"))
 
#food_genus_id    n
#1       1321.02 1249
#2       1322.02 1249
#3       1324.02 1249
#4       1530.06   53
#
#5   21113.02.01  880
#6   23991.01.02   16
#7   39120.04.01  807
   

## Loading data
   
ihs5 <- read.csv(here::here("inter-output",  "ihs5-fct_v1.2.csv")) %>% select(-X)
   
source("MAPS_Dictionary-Protocol.R")

   
#---ihs5-quality-check
   
#1) Checking that ihs5 and genus matches are correct. 

#Creating the list of ihs5 food items and its genus code
  
   
#Fixing samosa vs banana cake id typo issue (see MAPS_Dictionary-Protocol update v2.5)
ihs5$food_genus_id[ihs5$ihs5_foodid == "836" & ihs5$food_genus_id == "F0022.06"] <- "F0022.07"

# Changing 812 Yeast, baking powder, bicarbonate of soda to the correct code
# And removing yeast from the matches (it's was the same item (duplication))
ihs5$food_genus_id[ihs5$ihs5_foodid == "812"] <- "F1232.09"
ihs5 <- subset(ihs5, FoodName_3 != "yeast, baking")
# Fixing hot sauce code
ihs5$food_genus_id[ihs5$ihs5_foodid == "814"] <- "F1232.08"

#Changing values of meal eaten out & meat vendor
subset(ihs5, ihs5_foodid %in% c("829", "825"))
#Meal eaten at restaurant (vendor) c(21116.02, 21121.04) -->  F1061.01 
#TODO: Generate a recipe c(F1061.01,F1232.05 )
ihs5$food_genus_id[ihs5$ihs5_foodid == "829"] <- "F1061.01"

dim(ihs5)
#checking that all the ihs5 food items matches make sense  
ihs5_genus <- ihs5 %>% select(starts_with("ihs5"), food_genus_id) %>% 
 left_join(., dictionary.df %>% select(ID_3, FoodName_3), by = c("food_genus_id" = "ID_3")) %>% 
 arrange(ihs5_foodid)

# Checking for duplicates   
dim(ihs5_genus)

#Checked one by one all the matches and noted down the combination of codes that
#did not make sense for further checking

#836 - F0022.06 is no longer printing because solved above
#I left it for record keeping

ihs5_genus %>% 
 filter(ihs5_foodid %in% c("816", "832" , "836", "106", "510") &
          food_genus_id %in% c("F0666.01", "1530.07", "F0022.06",
                                   "23161.01.01", "21119.01.01"))


#removing matches that doesn't make sense in the ihs5-genus match    
ihs5_genus <- ihs5_genus %>%
     filter(!(ihs5_foodid %in% c("816", "832" , "836", "106", "510") &
                food_genus_id %in% c("F0666.01", "1530.07", "F0022.06",
                                     "23161.01.01", "21119.01.01")))

subset(ihs5_genus, ihs5_foodid == "812")

# Items without matches
subset(ihs5_genus, is.na(FoodName_3))

#Adding 
#118 - MAIZE UFA RAW MADEYA (bran flour - unprocessed)
#39120.04.01 - maize bran, flour, raw 

#Saving a copy of the matches so Gareth can check it (and other)
#write.csv(ihs5_genus, 
#          here::here("output", "ihs5-genus_standard-food-list_v2.0.csv"), row.names = F)

#TO-DO:
#Fix fct_ihs5 genus id codes (i.e., 816 - 23670.01.01, 1530.07)
#Remove redundant items (mice, rice imported, etc. ) - this is not very dramatic step, but it would help
#to keep the fct clean.
#Dissagregate aggregated items

#2) Checking that fct_ihs5 and genus matches are correct. 

#removing matches that doesn't make sense   

#fct should only have all unique genus_id.

#checking if we have duplicate genus 
#removing ihs5_foods (because there could be dupli)
#saving genus_id of the items that are dupli.


x <- ihs5 %>% select(-starts_with("ihs5")) %>% distinct() %>%
   left_join(., dictionary.df %>% select(ID_3, FoodName_3),
             by = c("food_genus_id" = "ID_3")) %>% 
   relocate(c(FoodName_3.x, ref_fctcode, ref_fctitem), .after = food_genus_id) %>% select(1:4) %>% 
   separate_rows(c("ref_fctcode", "ref_fctitem"), sep = ";")  %>% 
   arrange(food_genus_id)

genus.double <- x %>% count(food_genus_id) %>% arrange(desc(n)) %>% 
   filter(n>1) %>% pull(food_genus_id)

x <- x %>% filter(food_genus_id %in% genus.double)

ihs5 %>% select(-starts_with("ihs5")) %>% 
   separate_rows(c("ref_source", "ref_fctcode", "ref_fctitem"), sep = ";")  %>%
   distinct() %>%
   filter()
      
      
food_genus_id  <- c("1215.02", "1215.03", "1219.01.01", "1322.01", "1323.01", "1359.9.01" ,
"1359.9.02", "1501.02", "1501.05",  "1505.01", "1505.02", "1505.04", "1505.05",
"1699.05", "21121.04", "21170.92.02", "21170.92.03", "21397.01.01", "21431.01",
"2161.01", "2162.01", "21631.01.01", "22251.01.01", "23140.03.01",
 "24310.02.01", "39120.04.01", "F0020.01", "F0020.02", "F0623.03") 

ref_fctcode <-  c("MW04_0011", "MW04_0021", "MW04_0014", "MW05_0014", "MW05_0018", "MW05_0013", 
"MW05_0005", "91010", "8010", "MW03_0020", "MW03_0031", "MW03_0023", "MW03_0047",
"13002", "15073", "MW03_0069", "MW03_0009", "4025", "MW05_0012",
"11_009", "11_003", "9013", "6005", "1009", 
"12_002", "MW01_0035", "MW01_0004", "MW01_0003", "MW01_0061")

good.matches <- bind_cols("food_genus_id" = food_genus_id, "ref_fctcode" = ref_fctcode)



#Checking those without duplication

x <- x %>% filter(!food_genus_id %in% genus.double) %>% arrange(food_genus_id)

genus.double <- ihs5 %>% select(-starts_with("ihs5")) %>% distinct() %>% 
  count(food_genus_id) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% pull(food_genus_id)
   

x <- ihs5 %>% select(-starts_with("ihs5")) %>% distinct() %>%
   filter(food_genus_id %in% genus.double) %>%
   left_join(., dictionary.df %>% select(ID_3, FoodName_3),
            by = c("food_genus_id" = "ID_3")) %>% 
   relocate(c(FoodName_3, ref_fctcode, ref_fctitem), .after = food_genus_id) %>% 
   arrange(food_genus_id) %>% select(1:4)

dictionary.df %>% filter(ID_3 %in% c("23170.02.01", "23170.02.02"))
 