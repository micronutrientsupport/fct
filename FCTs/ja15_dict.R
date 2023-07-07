

#Loading data and library needed

#Loading the standardised FCT
ja15 <- read.csv(here::here("FCTs", "JA15_FCT_FAO_Tags.csv"))
#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}
#Loading functions
source(here::here("functions.R"))


names(ja15)
names(dictionary.df)

genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
"2022", "1540.02", "h", 
"2023", "1540.03", "h", 
"2025", "1540.04", "h", 
"10117", "1516.03", "m", 
"10118", "1520.11", "m", 
"10161", "1531.01", "h", 
"5041", "1441.01", "m", 
"14003", "21691.01.01", "h", 
"16019", "2413.01", "h", 
"3002", "23511.02.01", "m", 
"2008", "1530.05", "l",  #roasted
"11238", "21170.01.01", "h", 
"15107", "23670.01.05", "h",
"1136", "112.03", "m", 
"1001", "1199.9.02", "h", 
"6124", "1243.01", "h", 
"6156", "1253.02.03", "h", 
"6064", "1212.05", "h", 
"6184", "21399.02.01", "m", 
"7114", "1349.2.01", "h", 
"7012", "1354.01", "h", 
"7073", "1359.9.06", "h", 
"11204", "21116.02", "h", 
"11206", "21122.01", "h", 
"11240", "21170.01.02", "h", 
"11207", "21170.01.03", "h", 
"13033", "22251.01.04", "h", 
"7102", "21491.01", "h", 
"10289", "1562.03", "m", 
"10303", "1562.04", "m",
"6239", "1699.13", "h", 
"17078", "1699.14", "h", 

)

genus$ID_3 <- as.character(genus$ID_3)


(dupli <- genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))

#Updating the dictionary compilation -----
#for further use (to update versions) - first by alphabetic order
v <- "1.3.0"
genus %>% mutate(fct = "JA15")  %>% 
  write.csv(., here::here("metadata",
                          paste0("dict_fct_compilation_v.",v, ".csv")), 
            row.names = F)

#Adding food dictionary codes to FCT ----

ja15 <- ja15 %>% mutate_at("fdc_id", as.character) %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(ja15)
names(ja15)

## CHECK: Adding new food dictionary code ----

#Checking dictionary/ fct ids availability 
subset(ja15, fdc_id == "11204", select = c(food_desc)) 
subset(ja15, fdc_id %in% c("2022", 
                                   "2023",
                                   "2025"), select = c(food_desc)) 
subset(ja15, ID_3 == "21156.01") 
subset(dictionary.df, ID_3 == "21116.01")
