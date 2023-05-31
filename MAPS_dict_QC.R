
# Loading the dictionary
dict.df <- readRDS(file = here::here("inter-output", "dictionary.df.rds"))

#Checking higher class categories (ID_0) - cat. == 7
dict.df %>% group_by(ID_0, FoodName_0) %>% count() %>% arrange(desc(n))

#Empty cells 
unique(subset(dict.df, is.na(ID_0)))

#Checking categories (ID_1) by ID_0
id0 <- unique(dict.df$ID_0)
dict.df %>% filter(ID_0 == id0[1]) %>% 
  group_by(ID_1, FoodName_1) %>%  distinct(ID_1, FoodName_1) %>% knitr::kable()

subset(dict.df, ID_1 == "2562")

#Checking categories (ID_2) by ID_1, ID_0
id1 <- unique(dict.df$ID_1[dict.df$ID_0 == id0[4]])
dict.df %>% filter(ID_1 == id1[2]) %>% 
  group_by(ID_2, FoodName_2) %>%  distinct(ID_2, FoodName_2) %>% knitr::kable()

dict.df %>% 
  group_by(ID_1, FoodName_1, ID_2, FoodName_2) %>%  distinct(ID_2, FoodName_2) %>% count() %>% 
  arrange(desc(n))

subset(dict.df, ID_2 == "23110")

#Checking categories (ID_3) by ID_2
dict.df %>% 
  group_by(ID_2, FoodName_2, ID_3, FoodName_3) %>%  distinct(ID_3, FoodName_3) %>% count() %>% 
  arrange(desc(n))

#ID_3 & FoodName_3 must be unique
sum(duplicated(dict.df$ID_3[!is.na(dict.df$ID_3) & dict.df$ID_3 != ""]))
sum(duplicated(dict.df$FoodName_3[!is.na(dict.df$ID_3) & dict.df$ID_3 != ""]))
x <- which(duplicated(dict.df$ID_3[!is.na(dict.df$ID_3) & dict.df$ID_3 != ""]))

n1 <- dict.df$ID_3[!is.na(dict.df$ID_3) & dict.df$ID_3 != ""][x]

subset(dict.df, ID_3 %in% n1)
subset(dict.df, ID_3 == "23161.01.01")

#Run this to over-write any new upgrades in adding new food dictionary codes
#in dictionary folder


#IMPORTANT: to keep all folder updated run this code!!
#Saving Food-Dictionary into all r-project that use it

#here we are checking where the previous version was stored:
#
rproject.path <- str_remove(getwd(), "\\/[:alpha:]{1,}$")

rproject.path <- str_extract(getwd(), "C:/Users/[:alpha:]+/")

#knowing all the MAPS_Dictionary-Protocol.R

#list.files(rproject.path, pattern = "dictionary.df.rds", recursive = TRUE)


#
##IMPORTANT: This file needs to be updated in Teams!!
##This will only be necessary when a new release is made. 

##Saving a copy of the master file for MAPS

current_version <- str_extract(sort(list.files(here::here("output"), "MAPS_Dictionary"), 
                 decreasing = TRUE)[1],
            "(?<=v)[:graph:]{2,}(?=\\.)") 

version <- "3.0.2"

if(current_version < version){

dict.df  %>% filter(str_detect(ID_3, "\\b")) %>% 
  select(ID_0:FoodName_1, ID_3, FoodName_3) %>% 
  rename(
    food_group_id = "ID_0",
    food_group_name = "FoodName_0",
    food_subgroup_id = "ID_1",
    food_subgroup_name = "FoodName_1",
    food_genus_id = "ID_3",
    food_genus_name = "FoodName_3") %>% 
  write.csv(here::here('output',
                       paste0("MAPS_Dictionary_v", version, ".csv")), 
            row.names = F) 
}else{
  stop("incorrect version")
}
