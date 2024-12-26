
# Loading packages
library(dplyr)

#Loading data 
us19 <- read.csv(here::here("FCTs",  "US19_FCT_FAO_Tags.csv"))
#us19$fdc_id <-  as.character(us19$fdc_id)

# Food ids from US19 used in NDNS AA analysis
data.df <- read.csv(here::here("data", "Food_ID.csv"))
#Checking AAs
names(us19)[81:99]
names(data.df)
#data.df$NDB.no <-  as.character(data.df$NDB.no)

data.df %>% left_join(., us19, by = c("NDB.no" = "fdc_id")) %>% 
  select(1:3,  names(us19)[81:99]) %>%  
  write.csv(., here::here("inter-output", "UK-NDNS_US19_AA-matches_v1.0.0.csv"), 
            row.names = FALSE)

