
## 3) MAPS type format ----

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

# Loading file data compilation
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[2]
# Loading the compilation 
dict_comp <- read.csv(here::here("metadata", file)) %>%
  mutate_at(c("ref_fctcode", "ID_3"), as.character) %>% 
  filter(fct != "Joy et al, 2015")

# Compo dataset
crop <- read.csv(here::here("FCTs", "crop-compo_FCT_FAO_Tags.csv"))
head(crop)

## Adding Dictionary codes (GENuS) code 

genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "EJ15_7", "1290.9.16", "m",
  "EJ15_15", "1290.9.06", "m", 
  "EJ15_29", "23120.03.02", "h", 
  "EJ15_36", "1701.04", "h", 
  "EJ15_37", "1270.01", "m", 
  "EJ15_41", "1239.01.02", "h", 
  "EJ15_13", "1701.02", "h", 
  "EJ15_62", "23120.03.01", "l", 
  "EJ15_63", "39120.04.01", "l"
  
)


genus <- crop %>% left_join(., dict_comp[, c("ref_fctcode", "ID_3", "confidence")],
                   by = c("water_ref" = "ref_fctcode")) %>% 
  filter(!is.na(ID_3)) %>%   rename(ref_fctcode = "fdc_id") %>% 
  # Excluding some foods w/ wrong ID_3
  filter(!ref_fctcode %in% c( "EJ15_13")) %>% 
  select(c("ref_fctcode", "ID_3", "confidence")) %>% 
  bind_rows(genus) %>% distinct() %>% arrange(desc(ref_fctcode))

# crop %>% left_join(., dict_comp[, c("fdc_id", "ID_3", "confidence")],
#                    by = c("comments" = "fdc_id")) %>% 
#   filter(!is.na(SEmcg), is.na(ID_3)) %>% View()


# Updating the dictionary compilation -----

#Updating the dictionary compilation -----
#for further use (to update versions) - first by alphabetic order
v <- "1.4.2" # Changed the rice Genus in Malawi

genus %>% mutate(fct = "Joy et al, 2015")  %>% 
  write.csv(., here::here("metadata",
                          paste0("dict_fct_compilation_v.",v, ".csv")), 
            row.names = F)

# genus %>% mutate(fct = ) %>% 
#   bind_rows(., dict_comp %>%
#               #Excluding the fct so we re-paste the new matches (avoid dupli and old codes)
#               filter(fct != "Joy et al, 2015")) %>% distinct() %>% 
#   write.csv(., here::here("metadata", file), row.names = F)


# Finding the codes in dict

subset(dictionary.df, grepl("pump", FoodName_3, ignore.case = T) &
         grepl("", FoodName_3, ignore.case = T))


