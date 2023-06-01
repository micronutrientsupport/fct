


# Load libraries
library(dplyr)

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

#Loading the FC library
if(sum(ls() == "fct_dict") == 0) {
  source(here::here("FCTs",  "MAPS_output.R"))}

# Load data (household - food matches)
file <- "MAPS_ihs4"
nct <- readxl::read_excel(here::here("inter-output", paste0(file, ".xlsx")), 1)

names(nct)
head(nct)

#names(nct)[12] <- "ID_3"
names(nct)[19] <- "ID_3"

# Checking food consumed by household
nct %>% filter(original_id == "0001c970eecf473099368557e2080b3e" & 
                 original_food_name == "Maize ufa refined (fine flour)") %>% 
  select(amount_consumed_in_g)


nct %>% filter(original_id == "0001c970eecf473099368557e2080b3e") %>% 
  count(original_food_name) %>% arrange(desc(n))

nct %>% 
  count(original_id) %>% pull(n) %>% as.numeric() %>% mean(., na.rm =TRUE)

# Checking NCT - dict matches

food_list <- nct %>% select(ID_3, original_food_name, food_name) %>% distinct()

food_list %>% 
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  filter(is.na(FoodName_3)) %>% View()


# Checking unmatched/wrong dict to food name

dictionary.df %>% 
  filter(grepl("tilap", FoodName_3) & 
           grepl("raw", FoodName_3)) %>% 
  select(FoodName_3, ID_3)


dictionary.df %>% 
  filter(ID_3 == "21111.02.03")

# Fixing some ID_3 (dict codes)

food_list$ID_3[food_list$ID_3 == "142.02000000000001"] <- "142.02"
food_list$ID_3[food_list$ID_3 == "141.02000000000001"] <- "141.02"
food_list$ID_3[food_list$ID_3 == "2211.0100000000002"] <- "2211.01"
food_list$ID_3[food_list$ID_3 == "2161.0100000000002"] <- "2161.01"
food_list$ID_3[food_list$ID_3 == "2162.0100000000002"] <- "2162.01"
food_list$ID_3[food_list$ID_3 == "01520.01.03"] <- "1520.01.03"
food_list$ID_3[food_list$ID_3 == "01520.01.01"] <- "1520.01.01"
food_list$ID_3[food_list$ID_3 == "01520.01.02"] <- "1520.01.02"
food_list$ID_3[food_list$ID_3 == "142.05000000000001"] <- "142.05"
food_list$ID_3[food_list$ID_3 == "23170.01"] <- "23170.01.01"
food_list$ID_3[food_list$ID_3 == "2413.0100000000002"] <- "2413.01"

# Updating some dict codes
food_list$ID_3[food_list$ID_3 == "1699.04"] <- "F1232.06"
food_list$ID_3[food_list$ID_3 == "1699.05"] <- "F1232.07"
food_list$ID_3[food_list$ID_3 == "21121.040000000001"] <- "F1061.02"

## Adding foods to multi-matches
food_list$ID_3[food_list$original_food_name == "Gathered wild green leaves"] <- c("1290.9.16, 1290.9.07, 1290.9.15")
food_list$ID_3[food_list$original_food_name == "Beef"] <- c("21111.01.01, 21111.02.03, 21184.01.02, 21111.02.01, 21111.02.02")
#food_list$ID_3[food_list$ID_3 == "1501.05"] <- c("1503.08, 1503.03")

food_list <- food_list %>% separate_rows(ID_3, sep = ",") %>% # separating multiple items from above into idnv. rows
  mutate(ID_3 = str_squish(ID_3)) # removing white space from the operation above

# Checking matches with FCT

names(fct_dict)
unique(fct_dict$source_fct)

food_list  %>% 
  left_join(., fct_dict %>% 
              filter(str_detect(ID_3, "\\b"))) %>% 
  filter(is.na(WATERg)) %>% 
                  View()

food_list  %>% 
  left_join(., fct_dict %>% 
              select(ID_3, WATERg) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  View()
  
fct_dict$WATERg <- as.numeric(fct_dict$WATERg)

fct_dict %>% 
  filter(grepl("mice|game", food_desc, ignore.case = TRUE) & 
           grepl("", food_desc, ignore.case = TRUE) &
         #  !is.na(ID_3) &
         #  WATERg >10 &
           grepl("", food_desc, ignore.case = TRUE)
         ) %>% 
  select(source_fct, fdc_id, food_desc, ID_3, Edible_factor_in_FCT, WATERg,
         ENERCkcal, ZNmg, 
         scientific_name) %>% View()

fct_dict %>% 
  filter(ID_3 == "24490.01")

fct_dict %>% filter(fdc_id %in% c("MW04_0012")) %>% View()
fct_dict %>% filter(grepl("^07_", fdc_id)) %>% View()

## UPDATED UP TO HERE!! ----------
f

# Fixing food composition code - These matches should be reviewed and
# updated if new FC values are available. 

# Cassava - roasted, maize bran flour
nct$ID_3[nct$ID_3 == "1520.01.03"] <-  "1520.01.05"
nct$ID_3[nct$ID_3 == "39120.04.01"] <-  "23120.03.01"
nct$ID_3[nct$ID_3 == "1501.06"] <-  "1501.11"
nct$ID_3[nct$ID_3 == "21170.92.02"] <-  "21183.02"

## To remove those codes from dict too
# Jew mallow to jute mallow (scientific name)

nct$ID_3[nct$ID_3 == "1214.05"] <-  "1290.9.15"
nct$ID_3[nct$ID_3 == "1215.03"] <-  "1290.9.10"
nct$ID_3[nct$ID_3 == "1214.06"] <-  "1290.9.16"
nct$ID_3[nct$ID_3 == "23670.01.02"] <-  "22270.06"
nct$ID_3[nct$ID_3 == "24230.03.02"] <-  "24310.02.01"
nct$ID_3[nct$ID_3 == "22290.01"] <-  "22290.05"
nct$ID_3[nct$ID_3 == "1699.06"] <-  "F1232.04"
nct$ID_3[nct$ID_3 == "1501.04"] <-  "1507.12"
nct$ID_3[nct$ID_3 == "F0623.02"] <-  "23670.01.05"
#nct$ID_3[nct$ID_3 == ""] <-  "1501.11"

# Better matches available
nct$ID_3[nct$ID_3 == "21170.92.02"] <-  "21183.02"
nct$ID_3[nct$ID_3 == "23140.03.02"] <-  "23140.03.01"
nct$ID_3[nct$ID_3 == "21170.92.03"] <-  "21183.03"
nct$ID_3[nct$ID_3 == "1501.02"] <-  "1501.09"
nct$ID_3[nct$ID_3 == "1802.02"] <-  "1802.01"


#New variable for the changed consumption
nct$amount_consumed_std_in_g <- 
  
  # Peanuts fresh to peanuts dried --> Important due to the water content. 
  # Adjust the quantity to dry E.g., 100g of fresh weight will be 100*(100-50)/(100-10)
  nct$amount_consumed_in_g[nct$ID_3 == "142.03"] <- nct$amount_consumed_in_g[nct$ID_3 == "142.03"]*(100-50)/(100-10)
nct$ID_3[nct$ID_3 == "142.03"] <- "142.01"

# Catfish from fresh to dried. --> Important due to the water content. 
# Adjust the quantity to fresh E.g., 100g of drt weight will be qty*(100-DW)/(100-FW)
#Water values based on catfish WA19(09_060) - 78g (FW) and MW19(MW03_0048) - 21g (DW)
nct$amount_consumed_in_g[nct$ID_3 == "1505.01"] <- nct$amount_consumed_in_g[nct$ID_3 == "1505.01"]*(100-21)/(100-78)
nct$ID_3[nct$ID_3 == "1505.01"] <- "1503.07"


# Getting total consumption of each food

total_nct <- nct %>% group_by(ID_3, original_food_name, food_name) %>% 
  summarise(consumtion = sum(amount_consumed_in_g)) %>%
  arrange(desc(consumtion)) 


names(fct_dict)
unique(fct_dict$source_fct)

y <- total_nct  %>% 
  left_join(., fct_dict %>% 
              select(ID_3, WATERg, FEmg, ZNmg) %>%
              filter(str_detect(ID_3, "\\b"))) 

y %>% filter(is.na(WATERg))
