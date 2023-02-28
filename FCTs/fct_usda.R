

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


#Loading data and library needed

#Loading the standardised FCT
output_table <- read.csv(here::here("inter-output", "US19_FCT_FAO_Tags.csv"))
#Loading the food dictionary
source(here::here("MAPS_Dictionary-Protocol.R"))
#Loading functions
source(here::here("functions.R"))
#Loading formatting data
var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()


names(output_table)
names(dictionary.df)


#usdafct <- as_tibble(NA)

#usdafct <- usdafct %>% mutate(code = "174815", 
#fooditem = "Alcoholic beverage, distilled, all (gin, rum, vodka, whiskey) 80 proof", 
#WATER = 66.6, 							
#ENERC1 = 231,
#VITA_RAE = 0,
#FE = 0.04,
#ZN =	0.04,
#comment = "VITA_RAE was assumed") %>% 
#  select(-value)



genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "6076" , "F1232.11", "h",
  "6081" , "F1232.12", "h",
  "6981" , "F1232.14", "h",
  "5335",  "F1061.03", "h",
  "9312",  "1319.03", "h", 
  "9145" , "1319.04", "h", 
  "25050", "01520.01.04", "m", 
  "14555", "2899.01.01", "m",
  "20005", "23140.05.01", "h",
  "15175", "1570.01", "h",
   "15166", "1570.03", "h", 
  "11940", "21340.01", "m",
  "11564", "1251.02", "h", 
  "19304", "23540.01", "h", 
  "6961", "F1232.04", "l",
  "2047", "1699.02", "h", 
  "11216", "1657.01", "h", 
  "43406", "F1232.06", "h", 
  "18369", "F1232.07", "l", 
  "14278", "23914.04", "h",
  "35238", "23914.05", "h", 
  "16088", "142.05", "m", 
  "4025", "F1232.01", "m",
  "14054", "F0666.02", "h", 
  "14181", "F0666.03", "h",
  "14083", "22290.06", "h", 
  "14177", "22290.07", "h", 
  "14182", "22290.08", "h", 
  "14318", "22290.09", "h", 
  "43369", "22290.10", "h", 
  "15020", "1514.03", "h",
  "16006", "F1232.23", "m",
  "13353", "F1232.24", "m", 
  "90480", "23210.04.01", "m", 
  "5332", "21121.02", "h", 
  "10219", "21113.02.02", "h", 
  
  
  )
  
#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v"),
             decreasing = T)[2]
genus %>% mutate(fct = "US19")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at("ref_fctcode", as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

output_table <- output_table %>% mutate_at("fdc_id", as.character) %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(output_table)
names(output_table)
  
  ## CHECK: Adding new food dictionary code ----
  
  #Checking dictionary/ fct ids availability 
  subset(output_table, fdc_id == "90480", select = c(fdc_id, food_desc, ID_3, WATERg)) 
  subset(output_table, fdc_id %in% c("7045",  "7046",  "7090",  "7906",  "7908",  "7909", "13353",  "43131"),
  select = c(fdc_id, food_desc, ID_3)) 
  subset(output_table, ID_3 == "90480") 
  
  subset(output_table, grepl("paw", food_desc, ignore.case = TRUE) &
           grepl("", food_desc, ignore.case = TRUE)
         , 
         select = c(fdc_id, food_desc, ID_3, WATERg, scientific_name))
  
  dictionary.df %>% filter(ID_3 == "23511.02.01")
  subset(dictionary.df, ID_2 == "1520.01")
  subset(dictionary.df, ID_1 == "2541")
  subset(dictionary.df, ID_0 == "PB")
  subset(dictionary.df, grepl("pig", FoodName_2, ignore.case = TRUE))
  subset(dictionary.df, grepl("sugar", Description1, ignore.case = TRUE))
  