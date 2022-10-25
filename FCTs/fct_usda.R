

###############################################################################
#                                                                             #
#                                                                             #
#                                                                             #
#                                                                             #
#                       USDA FCT                                              #
#                                                                             #
#                                                                             #
#                                                                             #
#                                                                             #
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################


library(tidyverse)


usdafct <- as_tibble(NA)

usdafct <- usdafct %>% mutate(code = "174815", 
fooditem = "Alcoholic beverage, distilled, all (gin, rum, vodka, whiskey) 80 proof", 
WATER = 66.6, 							
ENERC1 = 231,
VITA_RAE = 0,
FE = 0.04,
ZN =	0.04,
comment = "VITA_RAE was assumed") %>% 
  select(-value)