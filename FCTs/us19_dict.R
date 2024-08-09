

#Loading data and library needed

#Loading the standardised FCT
us19 <- read.csv(here::here("FCTs", "US19_FCT_FAO_Tags.csv"),
                         encoding = "latin1") 
#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}
          


names(us19)
names(dictionary.df)


#usdafct <- as_tibble(NA)

#usdafct <- usdafct %>% mutate(code = "174815", 
#fooditem = "Alcoholic beverage, distilled, all (gin, rum, vodka, whiskey) 80 proof", 
#WATER = 66.6, 							
#ENERC1 = 231,
#VITA_RAE = 0,
#FE = 0.04,
#ZN =	0.04,
#comment = "VITA_RAE was assumed") %>% 
#  select(-value)



genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "6076" , "F1232.11", "h",
  "6081" , "F1232.12", "h",
  "6981" , "F1232.14", "h",
  "5335",  "F1061.03", "h",
  "9312",  "1319.03", "h", 
  "9145" , "1319.04", "h", 
  "25050", "1520.01.04", "m", 
  "14555", "2899.01.01", "m",
  "20005", "23140.05.01", "h",
  "15175", "1570.01", "h",
   "15166", "1570.03", "h", 
  "11940", "21340.01", "m",
  "11564", "1251.02", "h", 
  "19304", "23540.01", "h", 
  "6961", "F1232.04", "l",
  "2047", "1699.02", "h", 
  "11216", "1657.01", "h", 
  "43406", "F1232.06", "h", 
  "18369", "F1232.07", "l", 
  "14278", "23914.04", "h",
  "35238", "23914.05", "h", 
  "16088", "142.05", "m", 
  "4025", "F1232.01", "m",
  "14054", "F0666.02", "h", 
  "14181", "F0666.03", "h",
  "14083", "22290.06", "h", 
  "14177", "22290.07", "h", 
  "14182", "22290.08", "h", 
  "14318", "22290.09", "h", 
  "43369", "22290.10", "h", 
  "15020", "1514.03", "h",
  "16006", "F1232.23", "m",
  "13353", "F1232.24", "m", 
  "90480", "23210.04.01", "m", 
  "5332", "21121.02", "h", 
  "10219", "21113.02.02", "h", 
  "19078", "F0666.04", "h", 
  "19163", "23670.01.04", "h", 
  "11670", "1652.03", "h", 
  "14238", "21439.9.05", "h",
  "14240", "21439.9.06", "h",
  "14241", "21439.9.07", "h",
  "14282", "21439.9.08", "h",
  "14327", "21439.9.09", "h",
  "14334", "21439.9.10", "h",
  "14341", "21439.9.11", "h", 
  "10994", "21181.01", "m", 
  "9149",  "1329.01", "h", 
  "19334", "2351F.01", "h",
  "43031", "F0666.05", "m", 
  "4058", "21691.07.01", "m",
  "11886", "21321.01", "m",
  "11655", "21329.01", "h", 
  "11170", "21399.03.01", "h", 
  "19162", "23670.01.01", "m", 
  "20028", "F1232.25", "h",
  "9032", "21419.01.01", "h", 
  "9094", "21419.02.01", "h", 
  "9291", "21412.01", "h", 
  "20004", "115.01", "h", 
  "19400", "F0623.06", "h", 
  "16090", "142.04", "h",
  "3186", "23991.01.07", "h", 
  "3190", "23991.01.08", "h", 
  "3193", "23991.01.11", "h", 
  "3212", "23991.01.09", "h", 
  "42285", "23991.01.10", "h", 
  "16138", "F1232.26", "m",
  "10109", "21521.01", "m", 
  "19107", "23670.01.03", "h", 
  "19217", "21439.9.04", "l", 
  "19283", "22270.06", "m", 
  "14021", "24490.03", "m", 
  "20062", "116.01", "h",
  "12220", "1441.01", "h", 
  "4037", "21691.01.01", "h", 
  "14037",  "2413.01", "m", 
  "24593", "1522.02", "h",
  "80200", "1587.01", "h",
  "9218", "1324.02", "h", 
  "17174", "21119.01.01", "l", 
  "5160", "21170.01.01", "m",
  "19300", "23670.01.05", "h",
  "11109", "1212.01", "h", 
 # "19807", "112.03", "m",  # Silencing this item because it has multiple missing values
  "11088", "1243.01", "h", 
  "16074", "1701.05", "m" , 
  "12151", "1375.01", "h", 
  "12131", "1379.9.01", "h", 
  "11291", "1253.01.01", "h", 
  "11112", "1212.05", "h", 
  "11587", "1290.9.08", "h", 
  "11457", "1215.01", "h", 
  "11505", "1290.9.02", "h", 
  "11165", "1654.01", "h", 
  "11264", "21397.01.01", "h", 
  "43217", "21399.02.01", "m", 
  "9174", "1349.2.01", "h", 
  "9316", "1354.01", "h", 
  "9190", "1359.9.05", "h", 
  "9286", "1359.9.06", "h", 
  "5139", "21122.01", "h", 
  "5152", "21170.01.02", "h", 
  "5157", "21170.01.03", "h", 
  "1012", "22251.01.01", "m", 
  "1015", "22251.01.03", "h",
  "16063", "1706.06", "h", 
  "16053", "1702.03", "h",
  "36602", "F1232.30", "l", 
  "9059", "1319.07", "h", 
  "9351", "23670.02.01", "h", 
  "9100", "F0623.07", "h",
  "9271", "21491.01", "h",
  "21108", "F1232.31", "h", 
  "15164", "1562.03", "m", 
 # "35028", "1562.04", "m", # removed bc it has many missing values
  "11297", "1699.13", "h", 
  "2029", "1699.14", "h", 
  "2012", "1654.02", "h",
  "2013", "1654.03", "h", 
  "90560", "2920.01", "h", 
  "18019", "F0022.07", "m", 
 "25027", "F1232.34", "h",
 "14315", "23999.01.01", "m",
 "4582", "21641.01.01", "h", 
 "14305", "24310.01.04", "h", 
 "5219", "21124.01", "m", 
 "19034", "112.04", "h", 
  "14154", "24490.04", "h",
 "7088", "F1232.36", "m", 
 "14604", "24490.05", "h", 
 "20020", "23120.03.03", "h", # Select this instead of 20016 (see docu)
 "16157", "23170.03.01", "h" )
  

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

genus %>% mutate(fct = "US19")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at(c("ref_fctcode", "ID_3"), as.character) %>%
              #Excluding the fct so we re-paste the new matches (avoid dupli and old codes)
              filter(fct != "US19"))  %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

us19 <- us19 %>% mutate_at("fdc_id", as.character) %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(us19)
names(us19)
  
## CHECK: Adding new food dictionary code ----
  
# Checking dictionary/ fct ids availability 
  subset(us19, fdc_id == "16157", select = c(fdc_id, food_desc, WATERg)) 
  subset(us19, fdc_id %in% c("20020", "20016"),
  select = c(fdc_id, food_desc, WATERg)) 
  subset(us19, ID_3 == "23140.05.01") 
  
  subset(us19, fdc_id %in% c("20020", "20016")) %>% View()
  
  subset(us19, grepl("infant|formu", food_desc, ignore.case = TRUE) & !is.na(VITA_RAEmcg) &
           grepl("", food_desc, ignore.case = TRUE) &
           !grepl("", food_desc, ignore.case = TRUE), select = 1:2)
  
  
  subset(us19, grepl("pigeon", food_desc, ignore.case = TRUE) &
           grepl("", food_desc, ignore.case = TRUE) 
           #!grepl("with", food_desc, ignore.case = TRUE) &
  
         , 
         select = c(fdc_id, food_desc, ID_3, WATERg, VITA_RAEmcg, scientific_name))
  
  dictionary.df %>% filter(ID_3%in% c("1654.02"))
  subset(dictionary.df, ID_2 == "1319")
  subset(dictionary.df, ID_1 == "2514")
  subset(dictionary.df, ID_0 == "FV")
  subset(dictionary.df, grepl("popcorn", FoodName_3, ignore.case = TRUE))
  subset(dictionary.df, grepl("sugar", Description1, ignore.case = TRUE))
  