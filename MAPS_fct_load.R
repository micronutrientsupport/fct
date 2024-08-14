
# Script that loads and add dict codes to FCTs

# Load libraries
library(tidyverse)
library(viridis)
source("functions.R")

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()

# Adjusting variable names to updated version (V.1.6 updated on 2021/10/13)
var.name <- gsub("phyticacid", "phytate", var.name)

# Generating and updating the FCT-dict matches for each FCT - Uncomment!
# Only need to be run the first time, and when pulling a new version/ update
# Indiv. FCT matches can be found in the FCT_dict.R)

#fcts <- list.files(here::here("FCTs") , "*_dict.R")
#eval(parse(text = paste0("source('FCTs/", fcts, "')")))

# Getting the most up-to-date file
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]
# Loading the file (dictionary-to-FCTs-matches)
dict_comp <- read.csv(here::here("metadata", file)) %>% 
  rename(source_fct = "fct", 
         fdc_id = "ref_fctcode")

dict_comp %>% count(source_fct) 

# 1) Loading all FCDBs into one single database ----

#finding all the cleaned FCTs/FCDBs from the output folder
list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, #so it is not taking the fcts in the folder
           full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), 
                   locale = locale(encoding = "Latin1")))  

# Loading all the cleaned FCTs/FCDBs into one single object (data.frame)
fct_cover <- list.files("FCTs/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))) 

# checking that we have loaded all the FCT/FCDBs (n=5)
fct_cover %>% count(source_fct) 
colnames(fct_cover)

fct_cover$fdc_id[fct_cover$source_fct %in% c("JA15", "LS06")] <- gsub("^0", "", fct_cover$fdc_id[fct_cover$source_fct %in% c("JA15", "LS06")])
fct_cover$fdc_id <- str_squish(fct_cover$fdc_id )
fct_cover$source_fct <- str_squish(fct_cover$source_fct )


# 2)  Merging dict codes to fcts codes ----

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3)) %>% count(source_fct)

fct_cover %>% left_join(., dict_comp) %>% 
  filter(!is.na(ID_3) & source_fct == "WA19") %>% pull(fdc_id)

fct_dict <- fct_cover %>% left_join(., dict_comp)
