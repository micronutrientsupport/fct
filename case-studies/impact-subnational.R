

library(dplyr)
library(ggplot2)

ihs4 <- read.csv(here::here("output",
                            "MAPS_ihs4_food-cons_v1.0.0.csv"))

region <- readRDS(here::here("metadata", "ihs4-region.RDS"))

names(ihs4)


data.df <- ihs4 %>% left_join(., dictionary.df, 
                   by = c("food_genus_id" = "ID_3")) 

data.df <- data.df %>% left_join(., region, 
                      by = c("household_id" = "HHID")) 
  

summary.data <- data.df %>%  
  group_by(ID_1, FoodName_1, region) %>% 
  summarise(n = n(),
          mean = mean(amount_consumed_in_g), 
          median = median(amount_consumed_in_g), 
            sd = sd(amount_consumed_in_g)) %>% 
  mutate(gross = n*mean ) %>% 
  arrange(FoodName_1)

summary_region <- summary.data %>% group_by(ID_1, FoodName_1) %>% 
  summarise(total_gross = sum(gross)) %>% ungroup() %>% 
  left_join(., summary.data) %>% 
  mutate(perc = gross/total_gross*100) 

write.csv(summary_region, here::here("inter-output", 
                                     "summary-region_mwi_2023-09-14.csv"), 
          row.names = FALSE)
  
boxplot(amount_consumed_in_g ~ FoodName_1, data.df )

ggplot(data.df %>% filter(ID_0 =="CE"),
  aes(amount_consumed_in_g, FoodName_1)) +
  geom_boxplot() 
#+
 # facet_wrap(~ID_0)

ggplot(data.df %>% filter(ID_1 =="2514"), 
       aes(amount_consumed_in_g, FoodName_3)) +
  geom_boxplot() 


