
## 3) MAPS type format ----

#Loading the food dictionary
if(sum(ls() == "dictionary.df") == 0) {
  source(here::here("MAPS_Dictionary-Protocol.R"))}


## Adding Dictionary codes (GENuS) code 

ken_genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "1004",   "F0022.03",    "m",
  "6001",   "22241.01.01", "h",
  "12004",  "F0665.01",    "m", 
  "12003",  "23912.02.01", "m", 
  "4016",   "1232.01"  ,   "m",
  "11001",  "2910.01"  ,   "h",
  "10010",  "1379.9.01" ,  "l", 
  # "13027",  "1699.02"   ,  "m", #salt, ionized 
  "15026",  "F0022.05"  ,  "h",
  "1031",   "23710.01"  ,  "m", 
  "13028",  "1699.03"   ,  "h",
  "11003",  "23520.01"  ,  "m",
  "12005",  "23914.02"  ,  "m",
  "13031",  "21399.01.01" ,"m",
  "12008",  "24212.02.01" ,"m",
  "6026",   "22230.01.01" ,"m",
  "10006", "1442.01", "h",
 # "5009", "1491.02.01", "l", # better as fruit
  "1041", "1199.9.01", "m",
  "1045", "111.01", "m", 
  "3001", "1701.05", "m", 
  "3010", "1703.01", "m",
  "13023", "1253.02.03", "h",
  "2009", "1510.01", "m", 
  "10009", "142.01", "l",
  "13006", "1652.01", "m", 
  "13007", "1652.02", "m", 
  "7009", "21121.03", "h", 
  "8010", "1503.03", "h",
  "1007", "F0020.01", "m",
  "6008", "22241.02.01", "h", 
  "1034" ,  "23161.01.01", "m",
  "13017", "1699.07", "h", 
  "3019", "1709.9.01", "m", 
  "10014", "1444.01", "m", 
  "10015", "1445.01", "m",
  "4019", "1212.04", "m",
  "4022", "1214.01", "h",
  "13019", "1252.01", "m", 
  "5024", "1317.01", "m", 
  "4011", "1251.01", "m", 
  "2005", "1801.01", "m",
  "8002", "1505.07", "h",
  "4034", "1290.9.02", "m", 
  "2004", "1313.01", "m", 
  "4001", "1215.02", "m", 
  "1023", "1290.01.01", "h", 
  "1051", "1290.01.03", "m", 
  "13024", "1253.01.01", "h",
  "6007", "22120.02", "h",
  "13011", "1654.01", "h", 
  "7020", "21113.02.01", "h", #assumed w/o bones bc EP = 1
  "15125", "F0022.08", "h", 
  "13015", "1699.10", "h", 
  "4013", "1290.9.04", "h", 
  "1005", "F0020.02", "m", 
  "5012", "1319.01", "m", 
  "7019", "21115.01", "m",
  "10003", "1460.02", "m",
  "9011", "21700.02.02", "m", 
  "9001", "F1243.01", "m",
  "5028", "1342.01.01", "m",
  "4014", "1235.04", "m", 
  "4037", "21399.02.01", "m", 
  "1030", "23710.02", "h",
  "15081", "23914.03", "m",
  "6019", "22110.02.01", "h",
  "5025", "1319.02", "h",
  "8011", "1553.02", "h",
  "5031", "1346.01", "h",
  "4021", "1254.01", "h", 
  "4012", "1213.01", "m", 
  "5027", "1345.01", "m",
  "5034", "1354.01", "h", 
  "7025", "21184.02.01", "m",#No info on prep.
  "7022", "21184.01.01", "m", #No info on prep.
  "4004", "1213.02", "h", 
  "9002", "21691.02.01", "h",
  "8012", "1527.02", "h",
  "4008", "1231.01", "h",
  "4009", "1231.02", "h",
  "4010", "1231.03", "h", 
  "15019", "F0020.06", "h", 
  "15020", "F0020.04", "h", 
  "15130" ,"F0020.05", "h",
  "15003", "F0022.04", "m", 
  "15025", "F0022.07", "m",
  "7001" , "21111.02.03", "h",
  "7002" , "21111.02.01", "l", #All other are w/o bones, we assumed the same 
  "4003",  "1290.9.06", "h",
  "4029", "1290.9.07", "h",
  "4038", "1290.9.08", "h",
  "1036", "F0022.02", "h",
  "3002", "1243.01", "h",
  "1024", "23120.05.01", "h", 
  "1026", "23120.05.02", "h", 
  "6009", "22270.01", "h", 
  "6010", "22270.02", "h",
  "6011", "22270.03", "h",
  "6012", "22270.04", "h",
  "6013", "22270.05", "h",
  "1038", "23120.06.01", "h", 
  "1040", "23120.06.02", "h",
  "8035", "1533.02", "h", 
  "11002", "1802.02", "h",
  "3005", "1701.05", "h", 
  "10008", "1372.01", "l", 
  "10002", "1460.01", "h", 
  "9007", "21641.01.01", "m",
  "5002", "1341.02", "h",
  "5007", "1314.01", "h", 
  "5008", "1314.02", "h", 
  "9003", "1523.01", "l", #no fish specified, we assumed cod bc it's the most common
  "1044", "23110.01", "m", 
  "13003", "F1232.09", "h",
  "3008", "1709.9.02", "h" , 
  "3017", "1709.9.03", "h" ,
  "4025", "21397.01.01", "h",
  "4002", "1235.03", "h",
  "4024", "21393.01.01", "h", 
  "1001", "1199.9.02", "h",
  "7008",  "21122.01", "h", 
  "7026", "21170.01.03", "h",
  "1012", "F0022.09", "h", 
  "1013", "F0022.10", "h", 
  "1014", "F0022.11", "h", 
  "1015", "F0022.12", "h",
  "1016" ,"F0022.13", "h",
  "5013", "21439.02.01", "h", 
  "5016", "21439.04.01", "h", 
  "5026", "21439.9.03", "h", 
  "5035", "21439.01.01", "h",
  "5018", "1349.2.01", "h",
  "5039", "1319.04", "h",
  "7015", "21156.01", "h",
  "7018", "21155.01", "h",
  "5005", "21419.99.01", "h", 
  "8015", "1520.01" , "h",
  "8016", "1520.02" , "h",
  "8017", "1520.03" , "h",
  "8037", "1520.04" , "h",
  "8038", "1520.05" , "h",
  "8039", "1520.06" , "h",
  "8018", "1507.01" , "h",
  "8019", "1507.02" , "h",
  "8020", "1507.03" , "h",
  "8021", "1507.04" , "h",
  "8022", "1507.05" , "h",
  "8023", "1507.06" , "h",
  "8024", "1507.07" , "h",
  "8025", "1507.08" , "h",
  "8026", "1507.09" , "h",
  "8027", "1507.10" , "h",
  "8028", "1507.11" , "h",
  "8029", "1507.12" , "h",
  "8030", "1507.13" , "h",
  "8031", "1557.01", "h", 
  "8033", "1557.02", "h", 
  "8034", "1533.04" , "h",
  "8036",  "1533.05", "h", 
  "8040",  "1533.06", "h", 
  "8041",  "1533.07", "h", 
  "8042",  "1533.08", "h", 
  "15063", "F1232.10", "h", 
  "4035",  "1234.03", "h", 
  "2015",  "1530.01", "m", 
  "5020", "1316.05", "h", 
  "7014", "21116.02", "h",
  "1037", "114.02", "h", 
  "1011", "F0022.01", "m", 
  "4023", "1270.01", "h", 
  "7017", "21170.01.02", "h", 
  "10012", "21495.02.01", "h", 
  "8004", "1501.10", "h",
  "8022", "1507.05", "h",
  "1006", "F0020.07", "h",
  "1060", "23161.02.03", "h", 
  "5029", "21491.01", "m", 
  "10007", "21422.01", "h", 
  "10011", "1375.01", "m", # Check EP (as in this fct is shelled)
  "6015", "22222.02.01", "h", 
  "6016", "22222.01.01", "h", 
  "6003", "22251.01.02", "h", 
  "6004", "22251.01.03", "h", 
  "6005", "22251.01.04", "h",
  "14001", "21170.92.07", "h",
  "14002", "21170.92.08", "h",
  "14004", "21170.92.09", "h",
  "1029", "39120.06.01", "h",
  "15134" ,  "F0875.02", "h",
  "6020",  "22290.04", "m", 
  "6021",  "22290.05", "m",
  "5038", "1221.01", "m", 
  "4015", "1290.9.05", "h",
  "3015", "1242.01", "h", 
  "3012",  "1241.9.01", "h", 
  "13033", "F1232.10", "m" , 
  "1033", "23161.02.02", "h",
  "1003", "F0022.15", "h",
  "5001", "1341.03", "h",   
  "5006", "1319.05", "m", 
  "5009", "1359.9.04", "h",
  "5011", "1316.02", "m",
  "5021", "1359.9.05", "h",
  "5032", "1359.9.06", "h", 
  "5033", "1353.01.01", "h",
  "5037" , "1359.9.07", "h", 
  "13020", "1699.11", "h", 
  "4006", "1212.05", "h", 
  "2012", "1290.9.09", "m", 
  "2010" ,"1290.9.11", "h", 
  "2011", "1290.9.12", "h", 
  "1022",  "23120.03.01", "m",#No info on fermentation
  "8005", "1503.01", "h", 
  "8013", "1514.02", "h",
  "8008", "1505.08", "h", 
  "2002", "1599.1.02", "h",
  "4017", "1233.01", "h", 
  "4018", "21393.9.05", "h", 
  "4027", "1235.01", "h", 
  "4031", "1290.9.13", "h", 
  "4033", "1290.9.14", "h",
  "15129", "23161.02.05", "h",
  "1059" , "23161.02.07", "h", 
  "15074", "23161.02.06", "h",
  "1065", "F1232.27", "h", 
  "1063", "F1232.28", "h",
  "3007", "F1232.29", "h"
)


ken_genus <- read.csv(here::here("inter-output", "kenfct_matches.csv")) %>% 
  filter(!FCT.code %in% c("7009", "10010")) %>% #removing chicken - wrong code (21121.02) and macadamia wrong confidence
  select(FCT.code, MAPS.ID.code, Confidence) %>% 
  mutate(confidence = case_when(
    Confidence == "high" ~ "h", 
    Confidence == "medium" ~ "m", 
    Confidence == "low" ~ "l", 
    TRUE ~ Confidence)) %>% select(-Confidence) %>%
  mutate_at("FCT.code", as.character) %>% 
  rename(ref_fctcode = "FCT.code", 
         ID_3 = "MAPS.ID.code") %>% 
  bind_rows(ken_genus) %>% distinct()

(dupli <- ken_genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

##Find a way to stop it running if dupli == TRUE
x <- if(length(dupli) == 0){NA}else{length(dupli)} 
#x <- if(sum(duplicated(ken_genus$ref_fctcode)) == 0){NA}else{sum(duplicated(ken_genus$ref_fctcode))} 

if(!(is.na(x)))stop("duplicated code")

ken_genus %>% filter(ref_fctcode %in% dupli) %>% arrange(desc(ref_fctcode))

#Fixing horse bean to broad bean code (but they are all fava vicia)
ken_genus$ID_3[ken_genus$ref_fctcode == "3001"] <-  "1702.02"
#Fixing rice - acc. to SUA for Kenya all milled rice was coded
#23161.02 (whether imported or produced), hence we are changing
ken_genus$ID_3[ken_genus$ref_fctcode == "1034"] <-  "23161.02.01"
#Fixing beef to acc. for fat content variability
ken_genus$ID_3[ken_genus$ref_fctcode == "7004"] <-  "21111.02.02"
#Amend baking powder (1699.05) --> F1232.07 
ken_genus$ID_3[ken_genus$ref_fctcode == "13002"] <-  NA #"F1232.07"
#Excluding the baking powder due to very high Ca (see documentation)
ken_genus <- subset(ken_genus, ref_fctcode != "13002")
#Fixing samosa dictionary code
ken_genus$ID_3[ken_genus$ref_fctcode == "15025"] <-  "F0022.06"
#Millet, bulrush == Pearl millet (see docu)
ken_genus$ID_3[ken_genus$ref_fctcode == "1025"]  <- "118.03"
#Cabbage white updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "4007"]  <- "1212.03"
#Tomato, red updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "4036"]  <- "1234.02"
#Sweet potato, brown skin updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "2013"]  <- "1530.08"
#Mango ripe updated dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "5019"]  <- "1316.04"
#Goat medium fat updated dict code & confidence
ken_genus$ID_3[ken_genus$ref_fctcode == "7016"]  <- "21116.03"
ken_genus$confidence[ken_genus$ref_fctcode == "7016"]  <- "h"
#Sorghum, grain, white updated dict code & confidence
ken_genus$ID_3[ken_genus$ref_fctcode == "1039"]  <- "114.03"
ken_genus$confidence[ken_genus$ref_fctcode == "1039"]  <- "h"
#Cassava updating dict code
ken_genus$ID_3[ken_genus$ref_fctcode == "2007"]  <- "1520.01.01"


#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

ken_genus %>% mutate(fct = "KE18") %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
            mutate_at("ref_fctcode", as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

# Checking dictionary/ fct ids availability ----

## Loading KE18

kenfct <- read.csv(file = here::here("FCTs", "KE18_FCT_FAO_Tags.csv"))
head(kenfct)
names(kenfct)

#Checking for duplicated in KE18
kenfct %>% filter(fdc_id %in% dupli) %>% arrange(desc(fdc_id)) %>% select(fdc_id, food_desc)

# Joining with the dictionary codes and ke18
kenfct <- kenfct %>% 
  left_join(., ken_genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(kenfct)

# Checking dictionary/ fct ids availability ----

kenfct %>% filter(fdc_id %in% c("6017",
                              "6018",
                              "6005",
                              "6006")) %>% .[, c(3:4)]

subset(kenfct, fdc_id %in% c("13033"), 
       select = c(fdc_id, food_desc, ID_3, scientific_name))

subset(kenfct, fdc_id == "5009", select = c(food_desc, ID_3, scientific_name)) 
subset(kenfct, ID_3 == "1359.9.04") 
subset(kenfct, str_detect(ID_3, "01520")) 

dictionary.df %>% filter(ID_3 %in% c("1359.9.04"))
subset(dictionary.df, ID_2 == "F1232")
subset(dictionary.df, ID_2 %in% c("1379.02"
))
subset(dictionary.df, ID_1 == "2533")
distinct(subset(dictionary.df, ID_0 == "CE"), select = FoodName_1)

subset(kenfct, grepl("bean", food_desc, ignore.case = TRUE) &
         grepl("", food_desc, ignore.case = TRUE),
       select = c(fdc_id, food_desc, scientific_name, WATERg, ID_3))
subset(kenfct, str_detect(fdc_id, "^4") &
         grepl("raw", food_desc, ignore.case = TRUE),
       select = c(fdc_id, food_desc, ID_3, food_group, scientific_name)) %>% View()
subset(kenfct, str_detect(scientific_name, "triloba"), 
       select = c(fdc_id, food_desc, ID_3, food_group, scientific_name))

subset(dictionary.df, grepl("bean", FoodName_3, ignore.case = T) &
         grepl("canned", FoodName_3, ignore.case = T))

subset(dictionary.df, grepl("cabba", scientific_name, ignore.case = T) &
         grepl("", FoodName_2, ignore.case = T))

kenfct %>% filter(!is.na(ID_3)) %>% count()

ken_genus %>% anti_join(kenfct, by = "ID_3")
