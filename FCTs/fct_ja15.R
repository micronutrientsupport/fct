

#Loading data and library needed

#Loading the standardised FCT
output_table <- read.csv(here::here("inter-output", "JA15_FCT_FAO_Tags.csv"))
#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}
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
"10117", "1516.03", "m", 
"10118", "1520.11", "m", 
"10161", "1531.01", "h"

)


#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]
genus %>% mutate(fct = "JA15")  %>% 
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
subset(output_table, fdc_id == "10161", select = c(food_desc)) 
subset(output_table, fdc_id %in% c("2022", 
                                   "2023",
                                   "2025"), select = c(food_desc)) 
subset(output_table, ID_3 == "2351F.01") 
subset(dictionary.df, ID_2 == "1531")
