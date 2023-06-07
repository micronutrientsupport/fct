

# Load libraries
library(dplyr)


head(food) 


# Selecting MAPS variables

standard_names <- c("household_id", "original_food_id", "original_food_name",
                     "food_genus_id", "food_genus_confidence", 
                    "amount_consumed_in_g")

food %>% select(HHID, item_code, item , ID_3, 
                food_genus_confidence , amount_consumed_std_in_g)