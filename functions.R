
#Script that loads all functions

#Fuzzy matcher
source(here::here("functions", "Fuzzy_Matcher.R"))
#Food component calculators
source(here::here("functions", "summary_table_functions.R"))
#No brackets
#The following f(x) removes []
no_brackets <- function(i){
  case_when(
    str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'), 
    TRUE ~ i)
}

#Replacing "Tr" to zero
TraceToZero <- function(x){
  x <- gsub("Trace|trace|Tr|tr|N", "0", x)
  return(x) 
}
#Remove "*" to avoid conversion to NA
RemoveStar <- function(x){
  x <- gsub("\\*", "", x)
  return(x) 
}


no_brackets_tr_ast <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    str_detect(i, '[*]')  ~ str_extract(i, "[:digit:]+"),
    TRUE ~ i)
}