



#getting the names of all the standard variables names, to filter them afterward

variables <- read.csv(here::here( "fct-variable-names.csv"))
var.name <- variables %>% select(Column.Name) %>% pull

source("MAPS_Dictionary-Protocol.R")