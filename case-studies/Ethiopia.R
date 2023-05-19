
library(dplyr)

eth <- read.csv(here::here('data', 'MAPS_ETHFCT.csv'))

head(eth)
names(eth)

table <- readxl::read_excel(here::here('data', "EFCT_4.xls"), skip=1)

head(table)
names(table)
class(table$Zinc)

subset(table, grepl("tef", `FOOD AND DESCRIPTION`, ignore.case = TRUE) &
         !grepl("\\+", `FOOD AND DESCRIPTION`, ignore.case = TRUE)) %>%
  mutate(Zn_dw = as.numeric(Zinc)*(100)/(100-as.numeric(Moisture))*10) %>% 
  pull(Zn_dw) %>%  mean()


subset(table, grepl("wheat", `FOOD AND DESCRIPTION`, ignore.case = TRUE) &
         !grepl("\\+", `FOOD AND DESCRIPTION`, ignore.case = TRUE)) %>%
  mutate(Zn_dw = as.numeric(Zinc)*(100)/(100-as.numeric(Moisture))*10) %>% 
  pull(Zn_dw) %>%  mean()
