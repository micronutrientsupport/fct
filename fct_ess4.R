


#Assigning standard matching code (from dictionary code) to ess5

dictionary.df %>% filter(str_detect(FoodName_3, "garlic"))
dictionary.df %>% filter(str_detect(FoodName_2, "kale"))
dictionary.df %>% filter(ID_1 == "2549")
dictionary.df %>% filter(ID_2 == "2549")

ess4_food_st <- tribble(
  ~ref_foodid, ~ref_fooditem,   ~ID_3, ~confidence,
  
   "107",   "                                     Rice      ",  "23161.01.01", "m",
   "  108 ","                                          Oats ", "23140.07.01", "m", 
   "  207", "                                        Vetch  ", "1709.01.01", "l", 
   "  208", "                                    Fenugreek  ", "1449.9.02", "m", 
   "  209", "                                    mung bean  ", "1709.9.01", "m", 
   "  210", "                     Processed pulses (Shiro)  ", "23170.03.01", "l",
   "  303", "                                       SESAME  ",  "1444.01", "m", 
   "  304", "                                   Sun Flower  ",  "1445.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1212.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.04", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1214.01", "m", 
   "  404", " kale, cabbage, Pumpikn Leaf, Lettuce, spinach ", "1215.01", "m", 
   "   406","                                         Garlic",
   "   407","                       Moringa/Shiferaw/Halloka",
   "   503","                                          Mango", "1316.01", "m", 
   "   504","                                         Papaya", "1317.01", "m", 
   "   505","                                        Avocado", "1311.01", "m", 
   "   608","                                         Carrot",
   "   609","                                       Beetroot",
   "   711","                                 Honey, natural", "2910.01", "m", 
   "   806","                                     Chat / Kat", NA, NA, 
   "   807","                                   Hops (gesho)", ,  )
