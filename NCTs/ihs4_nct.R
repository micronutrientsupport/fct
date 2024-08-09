


#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

# Getting the most up-to-date file
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]
# Loading the file (dictionary-to-FCTs-matches)
dict_comp <- read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")



food_list <- readRDS(here::here("inter-output", 
              sort(list.files(here::here("inter-output"), 
             "food-list_ihs4"), decreasing = TRUE)[1])) 

names(food_list)

food_list %>% select(code, item, ID_3) %>% distinct() %>% 
  left_join(., dictionary.df) %>% select(1:3, FoodName_3) %>% 
   filter(is.na(FoodName_3))

food_list %>% select(code, item, ID_3) %>% distinct() %>% 
  left_join(., dict_comp) %>% select(1:3, fdc_id) %>% 
  filter(is.na(fdc_id))
