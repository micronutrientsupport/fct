
#Loading data and library needed

#Loading the standardised FCT
dk19 <- read.csv(here::here("FCTs", "DK19_FCT_FAO_Tags.csv"))
#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}
#Loading functions
source(here::here("functions.R"))


names(dk19)
names(dictionary.df)

genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "537", "1620.01", "m", 
  "469", "1587.01", "m", 
  "732", "21124.01", "l", 
)


#Checking for duplicates

(dupli <- genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))

#Updating the dictionary compilation -----
#for further use (to update versions) - first by alphabetic order
 v <- "1.4.0"
 genus %>% mutate(fct = "DK19")  %>% 
   write.csv(., here::here("metadata",
                           paste0("dict_fct_compilation_v.",v, ".csv")), 
             row.names = F)

#Updating the dictionary compilation -----
#file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
#             decreasing = T)[1]
#
#genus %>% mutate(fct = "DK19")  %>% 
#  bind_rows(., read.csv(here::here("metadata", file)) %>%
#              mutate_at(c("ref_fctcode", "ID_3"), as.character)) %>% distinct() %>% 
#  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

names(dk19)
class(dk19$fdc_id) 
dk19$fdc_id <- as.character(dk19$fdc_id)

dk19 <- dk19 %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(dk19)

## CHECK: Adding new food dictionary code ----

#Checking dictionary/ fct ids availability 

