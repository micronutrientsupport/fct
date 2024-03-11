
# Loading libraries
library(dplyr)

# Loading Dictionary (previous version)
dictionary.df <- read.csv(here::here("metadata", "MAPS_Dictionary_v2.6.csv")) %>% 
  select(-starts_with("X"))

# Checking variables
names(dictionary.df)

# Adding new column
dictionary.df$scientific_name <- NA

names(dictionary.df)

# Dictionary code corrections ----

# Correcting ID_3
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.01"] <- "1520.01.01"
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.02"] <- "1520.01.02"
dictionary.df$ID_3[dictionary.df$ID_3 == "01520.01.03"] <- "1520.01.03"
#Typo that duplicated two contiguous food items
dictionary.df$ID_3[dictionary.df$FoodName_3 == "cake, banana"] <- "F0022.07"

# Correcting FoodName_3
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.01.01"] <- "rice grain, imported, white, dried, raw"
dictionary.df$FoodName_3[dictionary.df$ID_3 == "23161.02.01"] <- "rice grain, local, white, dried, raw"


# Correcting fish dict codes (ID_2) (See documentation)
dictionary.df$ID_2[dictionary.df$FoodName_2 == "freshwater fish, liver oil"] <- "15100"
dictionary.df$ID_2[dictionary.df$FoodName_2 == "pelagic fish, frozen, fillet"] <- "15300"

# Correcting oil/seed (ID_0, FoodName_0) (See documentation)
dictionary.df$ID_0[dictionary.df$ID_1 %in% c("2561", "2571", "2572","2579")] <- "OT"
dictionary.df$FoodName_0[dictionary.df$ID_1 %in% c("2561", "2571",
                                                   "2572","2579")] <- "Other foods"
#sugar
dictionary.df$FoodName_0[dictionary.df$ID_3 == "23511.02.01"] <- "Other foods"
dictionary.df$ID_0[dictionary.df$ID_3 == "23511.02.01"] <- "OT"



### Cereals (CE) ----

## ├├  barley (115.01) -----

food_desc <-  c("barley grain, dried, unrefined, raw")

other_name <- c(rep(NA, 1))

scientific_name <- c(rep("hordeum vulgare", 1))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "115.01"
desc1 <-  c("Barley")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├ millet (118) -----



## ├├  cereal preparations (23140.08) -----

food_desc <-  c("barley grain, dried, unrefined, raw")

other_name <- c(rep(NA, 1))

scientific_name <- c(rep("hordeum vulgare", 1))

taxon <- c(NA)
fex2_new <- c(NA)

# One input
id2 <- "115.01"
desc1 <-  c("Barley")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


### Pulses and Beans (PB) ----

## ├├ chick peas, dry (1703) -----

food_desc <-  c("chick peas, dried, raw")

other_name <- c("garbanzo")

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "1703"
scientific_name <- "cicer arietinum"
desc1 <-  c("Chick peas, dry This subclass is defined through the following headings/subheadings of the HS 2007: 0713.20.")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}


## ├├ vetches (1709.01) -----

food_desc <-  c("vetch, dried, raw")

other_name <- c(NA)

fex2_new <- c(rep(NA, 1))

#Fixed
id2 <- "1709.01"
scientific_name <- "vicia sativa"
desc1 <-  c("Vetches, species of Vicia sativa (spring/common vetch), dried. Used mainly for animal feed. (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name
}


### Fruits and Vegetables (FV) ----


## ├├  other vegetables, fresh n.e.c. (1290.9) -----

food_desc <-  c(
  "malabar spinach, leaves, fresh, raw",
  "spider plant, leaves, fresh, raw",
  "black nightshade, leaves, fresh, raw", 
  "native eggplant, fresh, raw",
  "taro, leaves, fresh, raw",
  "cow pea, leaves, fresh, raw",
  "sweet potato, leaves, fresh, raw",
  "celery, fresh, raw", "radish, fresh, raw",
  "hibiscus, leaves, fresh, raw", 
  "radish, round, red skin, fresh, raw",
  "radish, long, red skin, fresh, raw", 
  "swiss chard, leaves, fresh, raw", 
  "stinging nettle, leaves, fresh, raw", 
  "jute mallow, leaves, fresh, raw", 
  "blackjack, leaves, fresh, raw",
  "baobab, leaves, fresh, raw", 
  "moringa, leaves, fresh, raw")

other_name <- c("Vine (African) spinach (KE18)",
                "cat\\'s whiskers (Luni) (MW19)",
                rep(NA, 12), 
                "bush okra, leaves, dried raw (WA19), Leaves, jews mallow, raw, Corchorus trilocularis, (Denje)",
                rep(NA, 3))

other_info <- c("https://www.fondazioneslowfood.com/en/ark-of-taste-slow-food/nderema/#:~:text=Nderema%2C%20also%20known%20as%20vine,green%20or%20brownish%2Dpurple%20stems.", 
                rep(NA, 14),
                "https://www.healthbenefitstimes.com/blackjack/, (RESEWO, 2013)",
                rep(NA, 2))

scientific_name <- c("basella alba", "gynandropsis gynandra/cleome gynandra",
                     "solanum scabrum", "solanum macrocarpon",
                     "colocasia esculenta", "vigna unguiculata",
                     NA,  "apium graveolens", NA, 
                     "hibiscus sabdariffa", 
                     "raphanus sativus", 
                     "raphanus sativus", 
                     "beta vulgaris ssp. vulgaris", 
                     "urtica dioica", 
                     "corchorus spp.", 
                     "bidens pilosa",
                     "adansonia digitata",
                     "moringa stenopetala")

fex2_new <- c(rep(NA, 14), "A00NY#F28.A07HS", rep(NA, 2))

# One input
id2 <- "1290.9"
desc1 <-  c("This subclass includes the fresh vegetables not elsewhere classified. They may not be identified separately because of their minor relevance at the international level. Because of their limited local importance, some countries report vegetables under this heading that are classified individually by FAO (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- other_info[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  apple juice (21435.01)  ----

food_desc <-  c("apple juice, fresh, sweetened")
other_name <- c(NA)
scientific_name <- c(NA)

fex2_new <- c("A039M#F10.A077J")

# One input
id2 <- "21435.01"
taxon <- c(NA)
desc1 <-  c("Juice is obtained by mechanical extractors, or by pressing, and is then submitted to various processes. Unfermented, it may or may not be frozen. For direct consumption (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon
  dictionary.df[n1,15] <- taxon_ref
}


### Other foods (OT) ----

## ├├  Food preparations (incl. sauces) (F1232) -----

food_desc <-  c("mayonnaise", "soup", "potash", "chilli sauce", "maize porridge", 
                "yeast, baking", "baking powder", 
                "baking soda, powder", "groundnut sauce", 
                "soup, tomato, condensed, canned", 
                "stock cube, beef", "stock cube, chicken", 
                "stock cube, vegetables", "stock cube, low sodium", 
                "cowpea, fried cakes", "maize dough, fermened, from maize grain", 
                "maize porridge, fermented from white maize, cooked", 
                "soup, chicken, beer yeast, vegetables and fermented African locus beans", 
                "soup, cabbage and vegetables", 
                "cassava and unripe plantain, mashed, cooked",                  
                "cassava and ripe plantain, mashed, cooked", 
                "yam and cassava, mashed, cooked", 
                "baked beans", "luncheon beef",
                "couscous, wheat", "falafel", 
                "pasta, macaroni, refined, dried, boiled",
                "pasta, spaghetti, refined, dried, boiled", 
                "beans, shellie, canned", 
                "rice, egg, fried", 
                "burger, hamburger, takeaway",
                "kebab in pitta bread with salad", 
                "injera, teff grain, cooked")

scientific_name <- c(rep(NA,16), "zea mays", rep(NA, 7), "triticum durum",
                     rep(NA,11))

other_name <- c(rep(NA,7), "bicarbonate of soda", NA, NA, 
                "beef seasoning cube", "chicken seasoning cube", 
                "vegetable seasoning cube", "low Na seasoning cube" ,
                "boussan touba (Burkina Faso) (WA19), beans akara (Sierra Leone)", 
                "kenkey (WA19), agidi (stiff maize dough) (Sierra Leone)", 
                "ogi (WA19), pap (Sierra Leone)", 
                rep("recipe from Burkina Faso in WA19", 2), 
                rep("Banakou né (Burkina Faso) (WA19), Yebbe (Sierra Leone)", 3), 
                rep(NA, 5), "egg, fried rice", rep(NA, 5), 
                "Ethiopian flatbread")

fex2_new <- c(rep(NA, 5), "A049A#F02.A06CK$F01.A066J$F27.A049A", 
              "A048Q#F02.A06CG", 
              rep(NA, 29))

# Fixed input
id2 <- "F1232"
desc1 <-  "Including both crop and livestock products. Inter alia: homogenized composite food preparations; soups and broths; ketchup and other sauces; mixed condiments and seasonings; vinegar and substitutes; yeast and baking powders; stuffed pasta, whether or not cooked; couscous; and protein concentrates. Include inter alia: turtle eggs and birds' nests. (Unofficial definition)"
ref1 <-  c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  
  
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}

## ├├  other oil seeds, n.e.c. (1449.9) -----

food_desc <-  c("niger seeds, dried, raw", 
                "benniseeds, dried, raw")

other_name <- c( rep(NA, 2))

scientific_name <- c("guizotia abyssinica",
                     "sesamum radiatum")

taxon <- c( rep(NA, 2))

# One input
fex2_new <- c(NA)
id2 <- "1449.9"
desc1 <-  c("This subclass covers other oilseeds, oleaginous fruits and nuts that are not identified separately because of their minor relevance at the international level. It also includes tea seeds, grape pips and tomato seeds from which oil is extracted. It includes, inter alia: - Fagus sylvatica (beech nut) - Aleurites moluccana (candlenut) - Carapa guineensis (carapa seed) - Croton tiglium (croton seed) - Bassia latifolia (illipe seed) - Guizotia abyssinica (niger seed) - Licania rigida (oiticica seed) - Perilla frutescens (perilla seed) - Jatropha curcas (physic nut) - Shorea robusta (sal tree seed) - Pongamia glabra (pongam seed) - Astrocaryum spp. (tukuma kernel) (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}

## ├├  other stimulant, spice and aromatic crops, n.e.c. (1699) -----

#Manual inputs:
food_desc <- c( "fenugreek, dried, raw", 
                "hops, dried, raw", 
  "curry powder", 
               "ginger, dried, powder", 
               "grains of selim, pepper, dried",
               "parsley, leaves, fresh, raw", 
               "parsley, leaves, dried, raw")

other_name <- c(rep(NA, 4), "Guinea pepper, Uda (Ethiopian pepper) (WA19), parmanji (SL)",
                rep(NA, 2))
scientific_name <- c("trigonella foenum-graecum",
                     rep(NA, 3), "xylopia aethiopica",
                     "petroselinum crispum", 
                     "petroselinum crispum")

fex2_new <- c(rep(NA, 7))


# Fixed
id2 <- "1699"
desc1 <-  c("Other stimulant, spice and aromatic crops, n.e.c. This subclass includes: - saffron, Crocus sativus - turmeric, Curcuma, Indian saffron, Curcuma longa - dill and dill seeds, Anethum graveolens - curry powders and pastes - thyme, Thymus - bay leaves, Laurus nobilis - Guinea pepper, negro pepper, seeds of Xylopia aethiopica - angelica stems This subclass does not include: - locust beans (carobs), cf. 01356 - unroasted chicory roots, varieties Cichorium intybus sativum, cf. 01691 - unroasted chicory roots, varieties other than Cichorium intybus sativum, cf. 01961 - sugar cane, cf. 01802 - sweet sorghum, Sorghum saccharatum, cf. 01809 - guarana nuts, cf. 01930 - kava, cf.01930 - basil and basil seeds, Ocinum basilicum, cf. 01930 - apricot, peach and plum stones and kernels, cf. 21499 - roasted chicory and other roasted coffee substitutes, cf. 23912")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")


# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new[i]
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1[i]
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
}


### Roots and Tubers (RT) ----

## ├├  flour of roots and tubers nes (23170.02) -----

food_desc <-  c("bread, ensete pulp, fermented, raw", 
                "ensete, flour, raw"
                )

other_name <- c( "kocho (ETH-ess3)", 
                 "bula (ETH-ess3)")

scientific_name <- c( rep(NA, 2))

taxon <- c( rep(NA, 2))

# One input
fex2_new <- c(NA)
id2 <- "23170.02"
desc1 <-  c("Flour and meal produced from roots and tubers o/t potatoes and cassava (Unofficial definition)")
ref1 <-  c("https://www.fao.org/faostat/en/#data/SCL")
taxon_ref <- c(NA)

# Function: 
for(i in 1:length(food_desc)){
  
  id2 <- id2
  id3 <- tail(sort(dictionary.df$ID_3[dictionary.df$ID_2 == id2]), n=1)
  id3_new <-ifelse(is.na(id3)|id3 == "", paste0(id2, ".01"),
                   str_replace(id3, "[[:alnum:]]{1,3}$",
                               formatC(seq(from = str_extract(id3, "[[:digit:]]{1,3}$"), 99),
                                       width=2, flag=0)[2]))
  
  n1 <- dim(dictionary.df)[1]+1
  
  n2 <- ifelse(is.na(id3)|id3 == "", which(dictionary.df$ID_2 %in% id2),
               which(dictionary.df$ID_3 %in% id3))
  
  #New entry - generation:
  dictionary.df[n1,] <- dictionary.df[n2,]
  #New entry - population:
  dictionary.df[n1,7] <- id3_new
  dictionary.df[n1,8] <- fex2_new
  dictionary.df[n1,9] <- food_desc[i]
  dictionary.df[n1,10] <- desc1
  dictionary.df[n1,11] <- ref1
  dictionary.df[n1,12] <- other_name[i]
  dictionary.df[n1,13] <- scientific_name[i]
  dictionary.df[n1,14] <- taxon[i]
  dictionary.df[n1,15] <- taxon_ref
}


