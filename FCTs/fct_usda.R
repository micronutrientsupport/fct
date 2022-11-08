

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



genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "6076" , "F1232.11", "h",
  "6081" , "F1232.12", "h",
  "6981" , "F1232.14", "h",
  "5335",  "F1061.03", "h",
  "9312",  "1319.03", "h", 
  "9145" , "1319.04", "h", 
  )
  
  
  ## CHECK: Adding new food dictionary code ----
  
  #Checking dictionary/ fct ids availability 
  subset(output_table, fdc_id == "17-060", select = c(food_desc, ID_3)) 
  subset(output_table, ID_3 == "2351F.01") 
  
  dictionary.df %>% filter(ID_3 == "23914.05")
  subset(dictionary.df, ID_2 == "1359.9")
  subset(dictionary.df, ID_1 == "2782")
  subset(dictionary.df, ID_0 == "PB")
  subset(dictionary.df, str_detect(FoodName_2, "fruit"))
  