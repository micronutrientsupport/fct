

# Load libraries
library(dplyr)

# Loading data
#Specify the hces
hces <- "ihs4"
# Getting the most recent version of the food consumption file
file_name <- sort(list.files(here::here("inter-output"), 
                paste0("food-cons_", hces)), decreasing = TRUE)[1]

# Getting the most recent version
food <- readRDS(here::here("inter-output", file_name)) 

# Getting household data
hh_info <- read.csv(here::here("inter-output", "hces",
                              paste0(hces,".hh.csv")))

# Getting roster household data
rost_info <- read.csv(here::here("inter-output", "hces",
                               paste0(hces,".roster.csv")))
head(rost_info) 
names(rost_info)

hh_info <- hh_info[2:8]

hces_cons <- hh_info %>% 
  left_join(., food %>%
    filter(!is.na(amount_consumed_std_in_g)), by = "HHID")


# Selecting MAPS variables

standard_names <- c("household_id", "latitud", "longitud", "urbanity", 
                    "wealth_quintile", "household_expenditure",
                    "interview_date",   "original_food_id",
                    "original_food_name",
                     "food_genus_id", "food_genus_confidence", 
                    "amount_consumed_in_g")

hces_cons <- hces_cons %>% select(HHID, lat_modified, lon_modified, urban, sep, rexpaggpc,
                intdate,
                item_code, item , ID_3, 
                food_genus_confidence , amount_consumed_std_in_g) 

# Renaming
names(hces_cons) <- standard_names


# Saving the food consumption and food matches
# Checking version

file_name <- paste0("MAPS_", hces, "_food-cons_")

current_hces_cons <- read.csv(here::here("output", 
                                   sort(list.files(here::here("output"), 
                                                   file_name), decreasing = TRUE)[1])) 


if(sum(hces_cons != current_hces_cons, na.rm = TRUE)>0){
  stop("Difference in food list file, need new version")
  
} else {
  
  write.csv(hces_cons,
          here::here("output",
                     paste0(file_name, "v1.0.0.csv")), 
          row.names = FALSE)
  
}