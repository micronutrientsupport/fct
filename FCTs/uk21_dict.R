
#Loading data and library needed

#Loading the standardised FCT
uk21 <- read.csv(here::here("FCTs", "UK21_FCT_FAO_Tags.csv"))
#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}
#Loading functions
source(here::here("functions.R"))


names(uk21)
names(dictionary.df)

genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "11-886", "23110.02", "m", 
  "11-002" ,  "23140.05.01", "h", 
  "11-788",  "23140.07.01", "l", 
  "13-086", "1701.02", "l", 
  "14-340", "F0262.01", "l",
  "17-034", "2168.01", "h", 
  "17-039",  "2165.01", "h", 
  "17-031", "2166.01", "h", 
  "17-043", "21691.07.01", "m",
  "17-686", "34550.01", "m", 
  "14-384", "1321.02", "m", 
  "14-319", "1341.01", "m", 
  "17-165", "23914.01", "m", # Black tea average
  "17-749", "24310.01.01", "m", 
  "18-488", "21121.01", "h", 
  "18-600",  "21521.01", "l", 
  "16-007",  "1501.02", "m", 
  "16-439", "1532.01", "l",
  "13-340", "1594.01", "l", # nori, dried TBC
  "16-492", "1518.01", "m", 
  "13-582", "1212.01", "h",
 # "17-170", "1620.01", "l",  # tea leaves infusion - 
  "17-171", "23914.04", "h", 
  "17-172", "23914.05", "h",
  "14-125", "1329.01", "h", 
  "17-515", "F1232.11", "h" ,
  "17-726", "F1232.12", "h",
  "17-727", "F1232.13", "h", 
  "17-672", "24490.03", "h", 
  "12-390", "22270.06", "m", 
  "12-389", "21439.9.04", "m",
  "17-767", "24310.01.03", "h", 
  "11-824", "23999.02.01", "m", 
  "17-645", "23999.02.02", "m", 
  "17-642", "23999.02.03", "m",
  "13-293", "1270.02", "h", 
  "16-279", "1520.07", "m", 
  "13-607", "21329.01", "h",
  "13-382", "21321.01", "h",
  "19-649", "21181.01", "m",
  "17-784", "F0623.04", "h", 
  "17-343", "F0623.05", "h",
  "13-881",  "1651.02", "h", 
 "12-535", "22290.02", "h",
 "12-326", "22290.03", "h",
 "17-647", "23670.01.01", "h", 
 "13-209", "1520.01.05", "h", 
 "17-738", "24490.01", "m", 
 "18-327", "F1061.02", "h", 
 "17-107", "23670.01.05", "h",
 #"17-692", "112.03", "m", 
 "14-212", "21491.01", "h", 
 "13-056", "1709.9.03", "h", 
 "13-352", "1253.01.01", "h", 
 "13-190", "1212.05", "h", 
 "17-416", "21393.01.01", "h", 
 "13-530", "21399.02.01", "h", 
 "14-140", "1349.2.01", "h", 
 "14-168", "1359.9.05", "h", 
 "19-656", "21184.01.01", "h", 
 "19-510", "21184.02.01", "h", 
 "18-371", "21122.01", "l",  # reported fat as well
 "12-550", "22251.01.03", "h", 
 "13-116", "141.04", "m", 
 "13-648", "1702.03", "m", 
 "11-444", "F1232.30", "m", 
 "13-167", "1319.07", "h", 
 "14-097", "F0623.07", "h", 
 "19-544", "F1232.31", "h", 
 "19-526", "F1232.32", "h", 
 "16-497", "1562.03", "m", 
 "13-844", "1699.13", "h", 
 "13-845", "1699.14", "h", 
 "13-818", "1654.02", "h", 
 "13-875", "1654.03", "h", 
 "17-222", "24230.03.01", "m", 
 "11-1029", "F0022.07", "m", 
 "17-002", "23620.01", "h",
 "17-734", "23999.01.01", "m", 
 "17-041", "21641.01.01", "h", 
 "18-350", "21124.01", "h",
 "11-774", "23140.03.03", "h",
 "19-468", "F1232.35", "m", 
 "19-643", "F1232.36", "h", 
 "14-060", "1359.02.01", "h"
 
 )

#Combining codes from fuzzy matcher and manually added

genus <- read.csv(here::here("metadata", "ukfct_matches.csv")) %>% 
  select(fdc_id, ID_3, Confidence) %>% 
  mutate(confidence = case_when(
    Confidence == "high" ~ "h", 
    Confidence == "medium" ~ "m", 
    Confidence == "low" ~ "l", 
    TRUE ~ Confidence)) %>% select(-Confidence) %>%
  # mutate_at("FCT.code", as.character) %>% 
  rename(ref_fctcode = "fdc_id") %>% 
  bind_rows(genus) %>% distinct()

#Cassava updating dict code
genus$ID_3[genus$ID_3 == "01520.01.01"]  <- "1520.01.01"
# Coriander leaves updating code
genus$ID_3[genus$ID_3 == "1290.9.03"] <- "1654.01"

#Checking for duplicates

(dupli <- genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))


#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

genus %>% mutate(fct = "UK21")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at(c("ref_fctcode", "ID_3"), as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

uk21 <- uk21 %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(uk21)

## CHECK: Adding new food dictionary code ----

#Checking dictionary/ fct ids availability 
subset(uk21, fdc_id == "18-350", select = c(food_desc, ID_3)) 
subset(uk21, fdc_id %in% c("12-535", "12-326"), select = c(fdc_id, food_desc, ID_3)) 
subset(uk21, ID_3 == "23670.01.01") 

dictionary.df %>% filter(ID_3 == "23670.01.01")
subset(dictionary.df, ID_2 == "21124")
subset(dictionary.df, ID_1 == "2782")
subset(dictionary.df, ID_0 == "PB")

subset(uk21, 
       grepl("", food_desc, ignore.case = TRUE)&
      grepl("^17", fdc_id), 
        select = c(fdc_id, food_desc, ID_3, WATERg))

subset(uk21, grepl("Weet", food_desc, ignore.case = TRUE) &
       grepl("", food_desc, ignore.case = TRUE), 
       select = c(fdc_id, food_desc, ID_3, WATERg))

subset(dictionary.df, 
       grepl("sugar", FoodName_2, ignore.case = TRUE) &
         grepl("",  FoodName_3, ignore.case = TRUE))











