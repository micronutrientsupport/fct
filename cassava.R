
#loading the packages

library(tidyverse)

## loading data

source("dictionary.R")

source("wafct.R")

source("mafood.R")

source('kenfct.R')

fct_region <- read.csv(here::here("data",
                    "MAPS_regional-SSA-fct_v1.6.csv"))


fct_cassava <- wafct %>% filter(str_detect(fooditem, "Cassava"), !str_detect(fooditem, "leaves")) %>% 
  select(FCT, code, fooditem, WATER, FE, ZN) %>%
  bind_rows(., mwi_clean %>% filter(str_detect(fooditem, "Cassava|cassava"), 
                     !str_detect(fooditem, "leaves|Leaves|stew")) %>% 
  select(code, fooditem, WATER, FE, ZN) %>% 
  mutate(FCT = "MAFOODS")) %>% 

  bind_rows(., kenfct %>% filter(str_detect(fooditem, "Cassava|cassava"), 
                      !str_detect(fooditem, 
                                  "leaves|Leaves|stew|Sorghum|sorghum|Peas" )) %>% 
  select(code, fooditem, WATER, FE, ZN) %>%
  mutate(FCT = "KENFCT")) %>% 
  
  bind_rows(., fct_region %>% filter(str_detect(original_food_name, "Cassava|cassava")) %>% 
    mutate(original_food_name = case_when(
      region == "M" ~ "Cassava - Middle region",
      region == "W" ~ "Cassava - Western region",
      region == "S" ~ "Cassava - Southern region",
      region == "E" ~ "Cassava - Eastern region",
      TRUE ~ original_food_name)) %>% 
      select(original_food_name,  starts_with(c("fe", "zn"))) %>% 
  rename_all(., ~c("fooditem", "FE", "ZN")) %>%
  mutate(FCT = "REGIONAL", 
         WATER = 62.1))

fct_cassava %>% ggplot(aes(FCT, FE)) + geom_point()
  
fct_cassava %>% pivot_longer(., cols =c(WATER, FE, ZN),
                             names_to = "nutrient") %>% 
  filter(nutrient != "WATER") %>% 
  ggplot(aes(FCT, value, color = nutrient)) + geom_boxplot() +
  labs(x = "FCT", y = "(mg/100g-EP-FW) in cassava and products")

DW <- function(x,y){x*(100)/(100-y)}

fct_cassava %>% 
  mutate_at(c("FE", "ZN"), ~test(., WATER)) %>%
  pivot_longer(., cols =c(WATER, FE, ZN),
               names_to = "nutrient") %>% 
  filter(nutrient != "WATER") %>% 
  ggplot(aes(FCT, value, color = nutrient)) + geom_boxplot() +
  labs(x = "FCT", y = "(mg/100g-EP-DW) in cassava and products")


fct_cassava %>% 
  filter(FCT != "REGIONAL") %>% 
  ggplot(aes(WATER, FE, color = FCT)) + geom_point() +
  scale_x_reverse()




fbs <- read.csv(here::here("data", "MAPS_FBS_2014-2018_v1.0.csv"))
fbsh <- read.csv(here::here("data", "MAPS_FBSH_1961-2013_v1.0.csv"))

# Customizing the output (1)
pdf("cassava_supply_2014-2018_plot.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

png("cassava_supply_2014-2018_plot.png",         # File name
    width=600, height=350)

# Creating a plot
fbs %>% filter(str_detect(original_name, "cassava"),
               country_id %in% c("NGA", "MWI")) %>% 
  ggplot(aes(date_consumed, amount_consumed_in_g, color = country_id)) + 
  geom_line() +
  labs(y = "cassava and product supply (g/capita/day)")

# Closing the graphical device
dev.off() 


# Customizing the output (2)
pdf("cassava_supply_1961-2018_plot.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

png("cassava_supply_1961-2018_plot.png",         # File name
    width=600, height=350)

# Creating a plot
fbs %>% filter(str_detect(original_name, "cassava"),
               country_id %in% c("NGA", "MWI")) %>% 
  bind_rows(., fbsh %>% filter(str_detect(original_name, "cassava"),
                 country_id %in% c("NGA", "MWI"))) %>% 
  ggplot(aes(date_consumed, amount_consumed_in_g, color = country_id)) + 
  geom_line() +
  labs(y = "cassava and product supply (g/capita/day)")

# Closing the graphical device
dev.off() 


# Customizing the output (3)
pdf("cassava_nutrient_FW_plot.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

png("cassava_nutrient_FW_plot.png",         # File name
    width=600, height=350)

# Creating a plot
fct_cassava %>% pivot_longer(., cols =c(WATER, FE, ZN),
                             names_to = "nutrient") %>% 
  filter(nutrient != "WATER") %>% 
  ggplot(aes(FCT, value, color = nutrient)) + geom_boxplot() +
  labs(x = "FCT", y = "(mg/100g-EP-FW) in cassava and products")

# Closing the graphical device
dev.off() 

# Customizing the output (4)
png("cassava_nutrient_DW_plot.png",         # File name
    width=600, height=350)# Width and height in inches
    
# Creating a plot
fct_cassava %>% 
  mutate_at(c("FE", "ZN"), ~test(., WATER)) %>%
  pivot_longer(., cols =c(WATER, FE, ZN),
               names_to = "nutrient") %>% 
  filter(nutrient != "WATER") %>% 
  ggplot(aes(FCT, value, color = nutrient)) + geom_boxplot() +
  labs(x = "FCT", y = "(mg/100g-EP-DW) in cassava and products")

# Closing the graphical device
dev.off() 

# Customizing the output (4)
png("cassava-composition_table.png",
    width=600, height=350)

# Creating a plot
fct_cassava %>% 
  write.csv(., here::here("output", "nutrient-cassava_ssa.csv"))

# Closing the graphical device
dev.off() 



fbs %>% filter(country_id == "NGA", date_consumed == "2018") %>% 
  left_join(., MAPS_wafct, by = "food_genus_id") %>%
  filter(amount_consumed_in_g> 0, !is.na(original_food_id)) %>% count()


