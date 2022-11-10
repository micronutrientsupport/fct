

#Loading data and library needed

#Loading the standardised FCT
output_table <- read.csv(here::here("inter-output", "JA15_FCT_FAO_Tags.csv"))
#Loading the food dictionary
source(here::here("MAPS_Dictionary-Protocol.R"))
#Loading functions
source(here::here("functions.R"))
#Loading formatting data
var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()



names(output_table)
names(dictionary.df)

genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
"2022", "1540.02", "h", 
"2023", "1540.03", "h", 
"2025", "1540.04", "h", 

)


#Checking dictionary/ fct ids availability 
subset(output_table, fdc_id == "13-881", select = c(food_desc, ID_3)) 
subset(output_table, fdc_id %in% c("2022", 
                                   "2023",
                                   "2025"), select = c(food_desc, scientfic_name)) 
subset(output_table, ID_3 == "2351F.01") 