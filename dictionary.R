



#getting the names of all the standard variables names, to filter them afterward

variables <- read.csv(here::here( "fct-variable-names.csv"))
var.name <- variables %>% select(Column.Name) %>% pull



dictionary <- read.csv(here::here("metadata", 
                                  "MAPS_Dictionary_v2.5.csv")) %>% 
  select(-starts_with("X"))

#Fixing names and/ typos

dictionary <- dictionary %>% mutate(FoodName_3 = case_when(
  ID_3 == "22211.01" ~ "milk, cow, full-fat, powder, unfortified", 
  ID_3 == "24490.02" ~ "sugar sweetened beverage, cola",
  TRUE ~ FoodName_3))




#New items

dictionary <- dictionary %>% add_row(
  ID_0 = "FV",
  FoodName_0 = "Fruits and Vegetables", 
  ID_1 = 2617,
  FoodName_1 = "apples and products", 
  ID_2 = "21435.01", 
  FoodName_2 =  "apple juice",
  ID_3 = "21435.01.01",
  FE2_3 = "A039M#F08.A032J",
  FoodName_3 = "apple, juice, sweetened") 






#Run this to over-write any new upgrades in adding new food dictionary codes
#in dictionary folder

file.copy("dictionary.R", 
          "C:/Users/LuciaSegoviaDeLaRevi/OneDrive - London School of Hygiene and Tropical Medicine/MAPS/02_working-files/r-project/")
