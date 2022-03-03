################################################################################
#
#                          
#          Mozambique Food Composition Table (MOZFCT, 2011)
#
#
#
################################################################################



##0) DOWNLOADING MOZAMBIQUE FCT FROM 

#Only need to do it once!




##1) LOADING PACKAGES AND MOZAMBIQUE FCT 

library(dplyr)

#MACRO
readxl::read_xlsx(here::here("data", "MZ0014.xlsx"), sheet = 4)

#MICRO
readxl::read_xlsx(here::here("data", "MZ0014.xlsx"), sheet = 5)


readxl::read_xlsx(here::here("data", "MZ0014.xlsx"), sheet = 4) %>% 
  left_join(., 
        readxl::read_xlsx(here::here("data", "MZ0014.xlsx"), sheet = 5))
