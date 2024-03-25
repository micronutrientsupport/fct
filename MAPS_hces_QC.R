
# Load libraries
library(dplyr)

# Loading data
#Specify the hces
hces <- "ihs4"


# Checking hces matches
file_name <- paste0("MAPS_", hces, "_food-cons_")
hces_cons <- read.csv(here::here("output",sort(list.files(here::here("output"), 
                                          file_name), decreasing = TRUE)[1])) 

# Checking matches in the tool

hces_cons %>%
  left_join(., fct_dict %>% select(fdc_id, food_desc, source_fct, ID_3), 
            by = c("food_genus_id" = "ID_3")) %>%
  filter(is.na(fdc_id)) %>% distinct(food_genus_id)
