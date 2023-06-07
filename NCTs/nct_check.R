


# Load libraries
library(dplyr)

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

#Loading the FC library
if(sum(ls() == "fct_dict") == 0) {
  source(here::here("FCTs",  "MAPS_output.R"))}

# Load household food list
flist <- read.csv(here::here("inter-output", "hces", "ihs4.food.items.csv"))[,2:3]
head(flist)

# Load data (household - food matches)
file <- "MAPS_ihs4"
nct <- readxl::read_excel(here::here("inter-output", paste0(file, ".xlsx")), 1)

# Fixing food name (in ihs4 food list)
nct$original_food_name[nct$original_food_name == "Small animal ÃƒÆ’Ã‚Â¯Ãƒâ€šÃ‚Â¿Ãƒâ€šÃ‚Â½ rabbit, mice, etc."] <- "Small animal ï¿½ rabbit, mice, etc."
nct$original_food_name[nct$original_food_name == "Citrus ÃƒÆ’Ã‚Â¯Ãƒâ€šÃ‚Â¿Ãƒâ€šÃ‚Â½ naartje, orange, etc."] <- "Citrus ï¿½ naartje, orange, etc."

names(nct)
head(nct)

# Renaming dict code (food genus) variable to ID_3
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

# Generating the food list with dict codes (from survey data in the tool)

food_list <- nct %>% select(ID_3, original_food_name, food_name, food_genus_confidence) %>% 
  distinct()
head(food_list)

food_list %>% filter(grepl("citru", original_food_name, ignore.case = TRUE)) %>% pull(original_food_name)
flist %>% filter(grepl("pop", item, ignore.case = TRUE))

# Checking items reported in hces and in the tool 
flist %>% left_join(., food_list, by = c("item" = "original_food_name")) %>% 
  filter(is.na(ID_3) &           # missing ID_3 and/or foods in the tool
           !grepl("00$", code))   # (optional) removing food group/category

# Adding food genus to item codes reported in the food list in the tool (food code + ID3)
food_list <- flist %>% 
  left_join(., food_list, by = c("item" = "original_food_name"))

# Checking that all food reported consumed have a ID_3

food_list %>% filter(is.na(ID_3) &           # missing ID_3 and/or foods in the tool
                   !grepl("00$", code))

#Adding dict code & confidenceto those foods missing it (ID_3)
food_list$ID_3[food_list$code == "116"] <- "23991.01.01"
food_list$food_genus_confidence[food_list$code == "116"] <- "low"
food_list$ID_3[food_list$code == "314"] <- "141.01"
food_list$food_genus_confidence[food_list$code == "314"] <- "low"
food_list$ID_3[food_list$code == "708"] <- "23991.01.03, 23991.01.04"
food_list$food_genus_confidence[food_list$code == "708"] <- "medium"
food_list$ID_3[food_list$code == "835"] <- "112.03"
food_list$food_genus_confidence[food_list$code == "835"] <- "high"

food_list <- food_list %>% separate_rows(ID_3, sep = ",") %>% # separating multiple items from above into idnv. rows
  mutate(ID_3 = str_squish(ID_3))

# Checking dict matches (ID_3) - correct & existing

food_list %>% 
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  filter(is.na(FoodName_3) & !is.na(ID_3)) %>% View()


# Checking unmatched/wrong dict to food name

dictionary.df %>% 
  filter(grepl("pop", FoodName_3) & 
           grepl("corn", FoodName_3)) %>% 
  select(FoodName_3, ID_3)


dictionary.df %>% 
  filter(ID_3 == "21170.92.02")

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

# Removing duplicates/ unnecessary multiple-matches
food_list <- subset(food_list, ID_3 != "23161.01.01")

# Checking matches with FCT

names(fct_dict)
unique(fct_dict$source_fct)

food_list  %>% 
  left_join(., fct_dict %>% 
              filter(str_detect(ID_3, "\\b"))) %>% 
  filter(!is.na(ID_3) & is.na(WATERg)) %>% 
                  View()

food_list  %>% 
  left_join(., fct_dict %>% 
              select(ID_3, WATERg) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  View()
  
fct_dict$WATERg <- as.numeric(fct_dict$WATERg)

fct_dict %>% 
  filter(grepl("pop", food_desc, ignore.case = TRUE) & 
           grepl("corn", food_desc, ignore.case = TRUE) &
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


# Fixing food composition code - These matches should be reviewed and
# updated if new FC values are available, low Q matches. 

# Cassava - roasted, maize bran flour
food_list$food_genus_confidence[food_list$ID_3 == "1520.01.03"] <-  "low"
food_list$ID_3[food_list$ID_3 == "1520.01.03"] <-  "1520.01.05"
food_list$food_genus_confidence[food_list$ID_3 == "39120.04.01"] <-  "low"
food_list$ID_3[food_list$ID_3 == "39120.04.01"] <-  "23120.03.01"
food_list$food_genus_confidence[food_list$ID_3 == "1501.06"] 
food_list$ID_3[food_list$ID_3 == "1501.06"] <-  "1501.11"


## To remove those codes from dict too
# Jew mallow to jute mallow (scientific name)
food_list$food_genus_confidence[food_list$ID_3 == "1214.05"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "1214.05"] <-  "1290.9.15"
food_list$food_genus_confidence[food_list$ID_3 == "1215.03"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "1215.03"] <-  "1290.9.10"
food_list$food_genus_confidence[food_list$ID_3 == "1214.06"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "1214.06"] <-  "1290.9.16"
food_list$food_genus_confidence[food_list$ID_3 == "23670.01.02"] 
food_list$ID_3[food_list$ID_3 == "23670.01.02"] <-  "22270.06"
food_list$food_genus_confidence[food_list$ID_3 == "24230.03.02"]
food_list$ID_3[food_list$ID_3 == "24230.03.02"] <-  "24310.02.01"
food_list$food_genus_confidence[food_list$ID_3 == "22290.01"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "22290.01"] <-  "22290.05"
food_list$food_genus_confidence[food_list$ID_3 == "1699.06"]
food_list$ID_3[food_list$ID_3 == "1699.06"] <-  "F1232.04"
food_list$food_genus_confidence[food_list$ID_3 == "1501.04"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "1501.04"] <-  "1507.12"
food_list$food_genus_confidence[food_list$ID_3 == "F0623.02"] 
food_list$ID_3[food_list$ID_3 == "F0623.02"] <-  "23670.01.05"

# Better matches available
food_list$food_genus_confidence[food_list$ID_3 == "21170.92.02"] <-  "high"
food_list$ID_3[food_list$ID_3 == "21170.92.02"] <-  "21183.02"
food_list$food_genus_confidence[food_list$ID_3 == "23140.03.02"] <-  "high"
food_list$ID_3[food_list$ID_3 == "23140.03.02"] <-  "23140.03.01"
food_list$food_genus_confidence[food_list$ID_3 == "21170.92.03"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "21170.92.03"] <-  "21183.03"
food_list$food_genus_confidence[food_list$ID_3 == "1501.02"] <-  "medium"
food_list$ID_3[food_list$ID_3 == "1501.02"] <-  "1501.09"
#food_list$food_genus_confidence[food_list$ID_3 == "1802.02"] 
#food_list$ID_3[food_list$ID_3 == "1802.02"] <-  "1802.01"

## Adding foods to multi-matches
food_list$ID_3[food_list$item == "Gathered wild green leaves"] <- c("1290.9.16, 1290.9.07, 1290.9.15")
food_list$food_genus_confidence[food_list$item == "Gathered wild green leaves"] <- "medium"
food_list$ID_3[food_list$item == "Beef"] <- c("21111.01.01, 21111.02.03, 21184.01.02, 21111.02.01, 21111.02.02")
food_list$food_genus_confidence[food_list$item == "Beef"] <- "medium"
#food_list$ID_3[food_list$ID_3 == "1501.05"] <- c("1503.08, 1503.03")

food_list <- food_list %>% separate_rows(ID_3, sep = ",") %>% # separating multiple items from above into idnv. rows
  mutate(ID_3 = str_squish(ID_3)) # removing white space from the operation above

# Adding the weight (allocation factor for each one to many foods)

food_list %>% add_count(code)

food_list <- food_list %>% add_count(code) %>% 
  mutate(wt = 1/n)

# Saving the food list (IHS4) code and ID_3 
# Checking version

current_food_list <- readRDS(here::here("inter-output", 
                                sort(list.files(here::here("inter-output"), 
                                "MAPS_food-list_ihs4"), decreasing = TRUE)[1])) 


if(sum(food_list != current_food_list, na.rm = TRUE)>0){
  stop("Difference in food list file, need new version")
  
} else {
  
saveRDS(food_list,
        here::here("inter-output",  "MAPS_food-list_ihs4_v2.1.0.rds"))

}
  
## UPDATED UP TO HERE!! ----------

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
