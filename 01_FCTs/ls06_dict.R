
## 3) MAPS type format ----

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}


## Adding Dictionary codes (GENuS) code 


lso_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,             
  "30014",  "22290.01",    "l",
 # "140001", "24310.01.01",  "h", we are removing this item bc this FCT doesn't provide info on alc. 
 "50057", "1290.01.02", "m",
 "11021", "112.03", "m", 
 "80059", "1354.01", "h", )                                    # hence alc. beverages shouldn't be included. 

lso_genus$ID_3 <- as.character(lso_genus$ID_3)

# Checking for duplicates

(dupli <- lso_genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

lso_genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))



#Updating the dictionary compilation -----

file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

lso_genus %>% mutate(fct = "LS06") %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at(c("ref_fctcode", "ID_3"), as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

# Checking dictionary/ fct ids availability ----

## Loading LS06

lsfct <- read.csv(file = here::here("FCTs", "LS06_FCT_FAO_Tags.csv"))
head(lsfct)
names(lsfct)
lsfct$fdc_id <- as.character(lsfct$fdc_id)


#Checking for duplicated in KE18
lsfct %>% filter(fdc_id %in% dupli) %>% arrange(desc(fdc_id)) %>% select(fdc_id, food_desc)

# Joining with the dictionary codes and ke18
lsfct <- lsfct %>% 
  left_join(., lso_genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(lsfct)

# Checking dictionary/ fct ids availability ----


## FCT

subset(lsfct, fdc_id == "30014")

subset(lsfct, grepl("beer", food_desc, ignore.case = TRUE) &
         grepl("", food_desc, ignore.case = TRUE),
       select = c(fdc_id, food_desc, food_group, WATERg))


## Dictionary


dictionary.df %>% filter(ID_3 %in% c("112.03"))
subset(dictionary.df, ID_2 == "F1232")
subset(dictionary.df, ID_2 %in% c("1379.02"
))
subset(dictionary.df, ID_1 == "2533")
distinct(subset(dictionary.df, ID_0 == "CE"), select = FoodName_1)

subset(dictionary.df, grepl("beer", FoodName_3, ignore.case = T) &
         grepl("", FoodName_3, ignore.case = T))
