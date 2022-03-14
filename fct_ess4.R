
#New dataset from 2018-2019.

ess4_food_list <- read.csv(here::here("inter-output", "eth_fct_match_v1.csv")) %>% 
   select(1, 2) %>% mutate(
      ref_fooditem = str_replace(item_code, "[:digit:]{2,3}\\.", "")) %>% 
   mutate_at("ref_fooditem", str_squish) %>% select(-item_code)


ess4_food <- ess4_food_list  %>%  left_join(., ess3_food_list) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   mutate(
      ref_foodid = case_when(
         str_detect(ref_fooditem, "Wheat") ~"2",
         str_detect(ref_fooditem, "Barley") ~ "3", 
         str_detect(ref_fooditem, "Green chili") ~  "141", 
         str_detect(ref_fooditem, "Red pepper") ~   "142",
         str_detect(ref_fooditem, "Injera") ~   "195",
         str_detect(ref_fooditem, "bread") ~   "196",
         TRUE ~ ref_foodid)) 


ess4_food <- ess4_food %>% filter(!is.na(ref_foodid), is.na(ID_3)) %>% select(1:3) %>% 
   left_join(., ess3_food_list %>% select(-ref_fooditem)) %>% 
   bind_rows(., ess4_food %>% filter(!is.na(ID_3))) 


#Assigning standard matching code (from dictionary code) to ess4

dictionary.df %>% filter(str_detect(FoodName_3, "beet"))
dictionary.df %>% filter(str_detect(FoodName_2, "other vegetables"))
dictionary.df %>% filter(ID_1 == "2645") %>% distinct(FoodName_2)
dictionary.df %>% filter(ID_2 == "1659")

ess4_food_st <- tribble(
  ~ref_foodid, ~ref_fooditem,   ~ID_3, ~confidence,
  
   "107",   "                                     Rice      ",  "23161.01.01", "m",
   "  108 ","                                          Oats ", "23140.07.01", "m", 
   "  207", "                                        Vetch  ", "1709.01.01", "l", 
   "  208", "                                    Fenugreek  ", "1699.07", "m", 
   "  209", "                                    mung bean  ", "1709.9.01", "m", 
   "  210", "                     Processed pulses (Shiro)  ", "23170.03.01", "l",
   "  303", "                                       SESAME  ",  "1444.01", "m", 
   "  304", "                                   Sun Flower  ",  "1445.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1215.01", "m", 
   "   406","                                         Garlic", "1252.01", "m", 
   "   407","                       Moringa/Shiferaw/Halloka", "1699.08", "m", 
   "   503","                                          Mango", "1316.01", "m", 
   "   504","                                         Papaya", "1317.01", "m", 
   "   505","                                        Avocado", "1311.01", "m", 
   "   608","                                         Carrot", "1251.01", "h", 
   "   609","                                       Beetroot", "1290.9.01", "m", 
   "   711","                                 Honey, natural", "2910.01", "m", 
   "   806","                                     Chat / Kat", NA, NA, 
   "   807","                                   Hops (gesho)","1699.09" , "m") %>% 
   mutate_all(., str_squish) %>% 
left_join(., dictionary.df %>% distinct()) 


#We are removing "others" because we are not matching those items
#We are adding the items new in ess4

ess4_food_list %>% left_join(., ess4_food) %>% filter(!is.na(ID_3)) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   rename(ref_foodid_ess3 = "ref_foodid", 
          ref_foodid = "fcode") %>% 
   mutate_at("ref_foodid", as.character) %>% 
   bind_rows(., ess4_food_st) %>% distinct(ref_foodid)

#Checking that we are not missing any code...
ess4_food_list %>% filter( !str_detect(ref_fooditem, "Other")) %>%  distinct(fcode)


ess4_food_list <- ess4_food_list %>% left_join(., ess4_food) %>% filter(!is.na(ID_3)) %>% 
   filter( !str_detect(ref_fooditem, "Other")) %>% 
   rename(ref_foodid_ess3 = "ref_foodid", 
          ref_foodid = "fcode") %>% 
   mutate_at("ref_foodid", as.character) %>% 
   bind_rows(., ess4_food_st)



#Assigning 

#3) Food matching

#KENFCT, 2018
source("kenfct.R")

missig_values <-  ess4_food_list %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(energy_in_kcal)) %>% 
    select(1:2,4)

ess4_food_list %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_ken, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(zn_in_mg)) %>% 
   select(1:2,4)

dictionary.df %>% filter(ID_3 == "1699.08")

#WAFCT, 2019

source("wafct.R")

missig_values %>% filter(!is.na(ID_3)) %>% 
   left_join(., MAPS_wafct, by = c("ID_3" = "food_genus_id")) %>% 
   filter(is.na(energy_in_kcal)) %>% 
   select(1:2,4)
