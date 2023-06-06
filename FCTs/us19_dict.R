

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
  "3184", "23991.01.05", "h", 
  "3216", "23991.01.06", "h", 
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
  "3682", "23991.01.01", "m",
  "9218", "1324.02", "h", 
  "17174", "21119.01.01", "l", 
  "5160", "21170.01.01", "m",
  "19300", "23670.01.05", "h",
  "11109", "1212.01", "h", 
  "19807", "112.03", "m")
  
#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

genus %>% mutate(fct = "US19")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at(c("ref_fctcode", "ID_3"), as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

us19 <- us19 %>% mutate_at("fdc_id", as.character) %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(us19)
names(us19)
  
  ## CHECK: Adding new food dictionary code ----
  
  #Checking dictionary/ fct ids availability 
  subset(us19, fdc_id == "9218", select = c(fdc_id, food_desc, WATERg)) 
  subset(us19, fdc_id %in% c("19400"),
  select = c(fdc_id, food_desc, ID_3)) 
  subset(us19, ID_3 == "23140.05.01") 
  
  subset(us19, grepl("cream", food_desc, ignore.case = TRUE) &
           grepl("", food_desc, ignore.case = TRUE) 
           #!grepl("with", food_desc, ignore.case = TRUE) &
  
         , 
         select = c(fdc_id, food_desc, ID_3, WATERg, scientific_name))
  
  dictionary.df %>% filter(ID_3%in% c("F1232.23" ))
  subset(dictionary.df, ID_2 == "F1232")
  subset(dictionary.df, ID_1 == "2513")
  subset(dictionary.df, ID_0 == "FV")
  subset(dictionary.df, grepl("cous", FoodName_3, ignore.case = TRUE))
  subset(dictionary.df, grepl("sugar", Description1, ignore.case = TRUE))
  