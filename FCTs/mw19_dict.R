

##) MAPS type format -----

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}

#Checking dictionary codes
dictionary.df %>% filter(ID_3 == "1520.01.03")

#Adding GENuS code

mwi_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
"MW01_0010" ,  "1520.01.02", "h",
"MW01_0011" ,  "1520.01.01", "h",
"MW01_0013" ,  "1550.01", "h",
"MW01_0014" ,  "F0022.04", "h",
"MW01_0016" ,  "118.02", "h",
"MW01_0017" ,  "23170.01", "h",
"MW01_0018" ,  "23120.03.02", "h",
"MW01_0019" ,  "23120.03.01", "h",
"MW01_0025" ,  "1510.02", "h",
"MW01_0037" ,  "112.01", "h",
"MW01_0040" ,  "1290.01.03", "h",
"MW01_0041" ,  "F0022.02", "h",
"MW01_0048" ,  "1313.01", "h",
"MW01_0050" ,  "1510.01", "h",
"MW01_0058" ,  "23161.01.01", "m",
"MW01_0060" ,  "114.01", "h",
"MW01_0063" ,  "1530.02", "h",
"MW01_0065" ,  "1530.01", "h",
"MW01_0066" ,  "1530.04", "h",
"MW02_0004" ,  "1701.03", "h",
"MW02_0007" ,  "1706.02", "h",
"MW02_0010" ,  "142.02", "h",
"MW02_0012" ,  "141.02", "h",
"MW02_0014" ,  "142.01", "h",
"MW02_0015" ,  "142.05", "h",
"MW02_0017" ,  "1707.01", "h",
"MW02_0019" ,  "141.01", "h",
"MW03_0006" ,  "21111.01.01", "h",
"MW03_0010" ,  "F1061.01", "l",
"MW03_0011" ,  "21121.01", "h",
"MW03_0013" ,  "231.02", "h",
"MW03_0015" ,  "231.01", "h",
"MW03_0019" ,  "1533.01", "h",
"MW03_0030" ,  "1501.03", "h",
"MW03_0052" ,  "21116.01", "h",
"MW03_0059" ,  "2211.01", "h",
"MW03_0063" ,  "21115.01", "h",
"MW03_0064" ,  "21113.02.01", "h",
"MW03_0065" ,  "21170.01.03", "h",
"MW03_0067" ,  "21114.01", "h",
"MW04_0003" ,  "1212.02", "h",
"MW04_0004" ,  "1212.01", "h",
"MW04_0019" ,  "1214.04", "h",
"MW04_0020" ,  "1214.03", "h",
"MW04_0025" ,  "1270.01", "h",
"MW04_0030" ,  "1239.01.01", "h",
"MW04_0031" ,  "1253.02.01", "h",
"MW04_0034" ,  "1235.01", "h",
"MW04_0036" ,  "1234.01", "h",
"MW05_0001" ,  "1341.01", "h",
"MW05_0002" ,  "1311.01", "h",
"MW05_0004" ,  "1312.01", "h",
"MW05_0008" ,  "1316.02", "h",
"MW05_0016" ,  "1316.01", "h",
"MW05_0019" ,  "1317.01", "h",
"MW05_0021" ,  "1318.01", "h",
"MW06_0001" ,  "21700.02.01", "h",
"MW08_0007" ,  "1802.01", "h",
"MW08_0008" ,  "2899.01.01", "h",
"MW01_0031", "F1232.05", "l")


(dupli <- mwi_genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")


#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

mwi_genus %>% mutate(fct = "MW19")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at(c("ref_fctcode", "ID_3"), as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

mwi_genus <-  mwi_genus %>% left_join(., dictionary.df)

sum(duplicated(mwi_genus))

#Checking codes
names(mwi_genus)
subset(mwi_genus, is.na(FoodName_3))


# Checking dictionary/ fct ids availability ----

## Loading MW19

mwfct <- read.csv(file = here::here("FCTs", "MW19_FCT_FAO_Tags.csv"))
head(mwfct)
names(mwfct)

#Checking for duplicated in MW19
mwfct %>% filter(fdc_id %in% dupli) %>% arrange(desc(fdc_id)) %>% select(fdc_id, food_desc)

# Joining with the dictionary codes and MW19
mwfct <- mwfct %>% 
  left_join(., mwi_genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(mwfct)

# Checking dictionary/ fct ids availability ----

mwfct %>% filter(fdc_id %in% c("6017",
                                "6018",
                                "6005",
                                "6006")) %>% .[, c(3:4)]

subset(mwfct, fdc_id %in% c("13033"), 
       select = c(fdc_id, food_desc, ID_3, scientific_name))

subset(mwfct, fdc_id == "5009", select = c(food_desc, ID_3, scientific_name)) 
subset(mwfct, ID_3 == "1359.9.04") 
subset(mwfct, str_detect(ID_3, "01520")) 

dictionary.df %>% filter(ID_3 %in% c("1359.9.04"))
subset(dictionary.df, ID_2 == "F1232")
subset(dictionary.df, ID_2 %in% c("1379.02"
))
subset(dictionary.df, ID_1 == "2533")
distinct(subset(dictionary.df, ID_0 == "CE"), select = FoodName_1)

subset(mwfct, grepl("bean", food_desc, ignore.case = TRUE) &
         grepl("", food_desc, ignore.case = TRUE),
       select = c(fdc_id, food_desc, scientific_name, WATERg, ID_3))
subset(mwfct, str_detect(fdc_id, "^4") &
         grepl("raw", food_desc, ignore.case = TRUE),
       select = c(fdc_id, food_desc, ID_3, food_group, scientific_name)) %>% View()
subset(mwfct, str_detect(scientific_name, "triloba"), 
       select = c(fdc_id, food_desc, ID_3, food_group, scientific_name))

subset(dictionary.df, grepl("bean", FoodName_3, ignore.case = T) &
         grepl("canned", FoodName_3, ignore.case = T))

subset(dictionary.df, grepl("cabba", scientific_name, ignore.case = T) &
         grepl("", FoodName_2, ignore.case = T))

mwfct %>% filter(!is.na(ID_3)) %>% count()

mwi_genus %>% anti_join(mwfct, by = "ID_3")
