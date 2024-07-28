
## 3) MAPS type format ----

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}


## Adding Dictionary codes (GENuS) code 

ken_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,