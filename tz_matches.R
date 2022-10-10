
source("kenfct.R")
source("wafct.R")


#Codes to be added
paste0("22270.0", seq(1, 4, by = 1))

subset(dictionary.df, 
      str_detect(FoodName_3, "biscuit") 
      #&
      #str_detect(FoodName_2, "milk")
      )

subset(dictionary.df, 
       str_detect(scientific_name, "scomberomorus"))

subset(dictionary.df, ID_2 == "23162")
subset(dictionary.df, ID_3 == "1529.02")
subset(dictionary.df, ID_1 == "2625")
distinct(subset(dictionary.df, ID_1 == "2625", select = c(ID_2, FoodName_2)))
distinct(subset(dictionary.df, ID_0 == "CE", select = c(ID_1, FoodName_1)))

subset(dictionary.df, ID_2 == "F1061" &
         FoodName_2 == "meat of chickens, fresh or chilled")

code <- c( "1553",
 "1542",
 "1540",
"15291",
"15290",
 "1529",
"15271",
"15270",
 "1527",
 "1514",
"15030",
 "1503")

food <- "drop"

id1 <-  "1505"
id2 <-  "1290.9"

subset(dictionary.df, str_detect(FoodName_2, "juice"))
subset(dictionary.df, str_detect(FoodName_3, "bread"))
subset(kenfct, str_detect(fooditem, "Ice")) %>% pull(code)
subset(kenfct, str_detect(code, "5035"))

subset(kenfct, str_detect(ID_3, "23120.03.02"))
subset(wafct, str_detect(code, "12_012"))

subset(dictionary.df, ID_2 %in% id2)
subset(dictionary.df, ID_2 %in% code, select = c(FoodName_2, ID_2))

subset(dictionary.df, ID_2 == "2351F",
       select = "FoodName_2")


#### Vegetables others -------

vege <- subset(kenfct, foodgroup == "VEGETABLE AND VEGETABLE PROCUCTS")

#### Brown sugar -------

#### Capsicum -------



#### Leaves -------

subset(kenfct, str_detect(fooditem, "cow pea")  & str_detect(fooditem, "raw"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name))

subset(dictionary.df, str_detect(FoodName_3, "lea"), 
       select = c(FoodName_3, ID_3, FoodName_2))


#### Fish -------

###### wafct  -------

subset(wafct, str_detect(fooditem, "Fish") & str_detect(fooditem, "dried"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name))

subset(wafct, str_detect(foodgroup, "Fish") 
       #& is.na(scientific_name)
       ,
               select =  fooditem) %>% distinct() %>% pull()

subset(wafct, str_detect(foodgroup, "Fish") & str_detect(fooditem, " raw"), 
       select = c(code, fooditem, ID_3, foodgroup, scientific_name))

fish_code <- c("Labeo spp.",
"Engraulis encrasicolus",
"Bagrus spp.",
"Synodontis spp.",
"Clarias gariepinus",
"Lates niloticus" ,
"Oreochromis spp./Tilappia spp.",
"Sphyraena spp.",
"Scomberomorus spp.",
"Sardinella spp.",
"Family: Penaeidae" ,
"Thunnus spp.")

subset(wafct, 
       scientific_name %in% fish_code, select = code) %>% pull()

#FRESH WATER

"Labeo spp.", catfish, lake Tanganyika & sardine?, Lake Nyasa
"Engraulis encrasicolus", Engraulicypris sp., sardine, (Usipa/dagaa), Lake Nyasa
"Bagrus spp.", Bagrus docmak/Bagrus , Catfish (Mbofu, Kibogobogo)/ Sardine (Mbofu)/ Sardine (Vitoga?), Lake Tanganyika/ Lake Nyasa /Mtera dam
"Synodontis spp.", Synodontis lacustricolus/ Synodontis, Catfish (Ngogo/Gogogo/Kolokolo)/ Sardine (Ngogo)/ Sardine (Kambale), Lake Tanganyika/Lake Nyasa /Mtera dam 
"Clarias gariepinus", Clarias gariepinus/ Clarias theodorae/ Clarias liocephalus/Clarias gariepinus/ Clarias sp. , Catfish (Kambale,Mumi)/ Catfish (Kambale) (x3)/ Sardine (Ngogo), Lake Tanganyika/ Lake Victoria (x3)/ Mtera dam
"Lates niloticus" , Lates stappersii (Mgebuka/Mkeke/Mvolo)/ Lates anguistifrons (Sangara)/ Lates mariae (Sangara /Ng’omba)/ Lates microlepis (Sangara/Nonzi)/ Lates niloticus, Nile perch (x4)/ Nile perch (Sangara), Lake Tanganyika (x4)/ Lake Victoria
"Oreochromis spp./Tilappia spp.", Oreochromis tanganicae (Serotheron) (Ngege)/ Oreochromis niloticus (Sato/Perege)/ Oreochromis rukwaensis (Sasala)/ Oreochromis leucostictus (Satu, Ngege)/ Tilapi zillii (Sato)/Tilapia rendalli (Kayabo)/ Oreochromis sp. (Magege)/ Oreochromis urolepis (Perege) , Tilapia , Lake Tanganyika /Lake Victoria (x5)/Lake Nyasa/ Mtera dam


#MARINE

"Sphyraena spp.", Sphyrae na obtusata/ Sphyraenella chrysotaenia , NA/ Obtuse barracuda (Msusa/Mzia)
"Scomberomorus spp.", Scomberomorus plurilineatus, King fish (Nguru) 
"Sardinella spp.", Sardinella neglecta, East African sardinella (Dagaa-papa)
"Family: Penaeidae" , Penaeus bubulus, Giant tiger prawn (Kamba mti)
"Thunnus spp.", Thunnus obesus (Jodari macho makubwa) (tuna-like)


#NO REPORTED
"Gadus morhua" 
"Trachurus trachurus"
"Cyprinus carpio" 
"Callinectes spp." 
"Trachurus symmetricus" 
"Coryphaena hippurus" 
"Amblypharyngodon mola" 
"Mormyrus spp."
"Family: Buccinidae" 
"Polydactylus spp."
"Families: Palaemonidae/Penaeidae"
"Family: Palaemonidae"
"Family: Veneridae"  
"Epinephelus spp." 

###### kenfct  -------

#8008, Nile perch, dry, raw 

subset(kenfct, str_detect(fooditem, "Fish|fish") 
        & str_detect(fooditem, "dr")
       , 
       select = c(code, fooditem, ID_3, scientific_name))

subset(kenfct, str_detect(foodgroup, "FISH") 
       #& is.na(scientific_name) 
       ,
       select =  fooditem) %>% distinct() %>% pull()

subset(kenfct, str_detect(foodgroup, "FISH")  &  str_detect(fooditem, " raw") ,
       select =  c(code, fooditem, ID_3, foodgroup, scientific_name)) 

subset(kenfct, 
 scientific_name %in% c("Rastrineobola argentea",
  "Protopterus annectens",
  "Clarias gariepinus",
  "Lates niloticus" ,
  "Oreochromis niloticus",
  "Rastrelliger kanagurta",
  "Carcharhinus spp.",
  "Penaeidae" ,
  "Sardinella spp.",
  "Thunnus albacares/T. thynnus"), select = code)

#FRESH WATER

"Rastrineobola argentea", Rastrineobola argentae, Lake Victoria sardine (Dagaa), Lake Victoria
"Protopterus annectens", Protopterus aethiopicus, Protopterus (Kamongo, Kambale mamba), Lake Victoria

#These three are reported in wafct too
"Clarias gariepinus", Clarias gariepinus/ Clarias theodorae/ Clarias liocephalus/Clarias gariepinus/ Clarias sp. , Catfish (Kambale,Mumi)/ Catfish (Kambale) (x3)/ Sardine (Ngogo), Lake Tanganyika/ Lake Victoria (x3)/ Mtera dam
"Lates niloticus" , Lates stappersii (Mgebuka/Mkeke/Mvolo)/ Lates anguistifrons (Sangara)/ Lates mariae (Sangara /Ng’omba)/ Lates microlepis (Sangara/Nonzi)/ Lates niloticus, Nile perch (x4)/ Nile perch (Sangara), Lake Tanganyika (x4)/ Lake Victoria
"Oreochromis niloticus", Oreochromis tanganicae (Serotheron) (Ngege)/ Oreochromis niloticus (Sato/Perege)/ Oreochromis rukwaensis (Sasala)/ Oreochromis leucostictus (Satu, Ngege)/ Oreochromis sp. (Magege)/ Oreochromis urolepis (Perege) , Tilapia , Lake Tanganyika /Lake Victoria (x3)/Lake Nyasa/ Mtera dam


#MARINE
"Rastrelliger kanagurta", Rastrelliger kanagurta/ Restrelliger chrysozonus, NA (Vibua)/ Indian Mackerel(Vibua)
"Carcharhinus spp.",  Carcharhinus falciformis, Silky shark (Dagaa-Papa)

#These three are reported in wafct too
"Penaeidae" , Penaeus bubulus, Giant tiger prawn (Kamba mti)
"Sardinella spp.", Sardinella neglecta, East African sardinella (Dagaa-papa)
"Thunnus albacares/T. thynnus", Thunnus obesus (Jodari macho makubwa) (tuna-like)


#NO REPORTED
"Gadus spp." 
"Anguilla spp."
"Ilisha melastoma"



#### Beef -------

x <- kenfct$FAT[kenfct$code %in% c("7001", "7002", "7004")]
#x[4:6] <- wafct$FAT[wafct$code %in% c("07_014", "07_009", "07_002")]
y <- wafct$FAT[wafct$code %in% c("07_014", "07_009", "07_002")]
mean(y)

wafct$ref[wafct$code %in% c("07_014", "07_009", "07_002")]
kenfct$biblio_id[kenfct$code %in% c("7001", "7002", "7004")]
kenfct$EDIBLE[kenfct$code %in% c("7001", "7002", "7004")]
