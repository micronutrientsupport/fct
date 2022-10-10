
dict.df <- readRDS(file = here::here("inter-output", "dictionary.df.rds"))

#Checking higher class categories (ID_0) - cat. == 7
dict.df %>% group_by(ID_0, FoodName_0) %>% count() %>% arrange(desc(n))
#Empty cells 
unique(subset(dict.df, is.na(ID_0)))

#Checking categories (ID_1) by ID_0
id0 <- unique(dict.df$ID_0)
dict.df %>% filter(ID_0 == id0[7]) %>% 
  group_by(ID_1, FoodName_1) %>%  distinct(ID_1, FoodName_1) %>% knitr::kable()

subset(dict.df, ID_1 == "2543")

#Checking categories (ID_2) by ID_1, ID_0
id1 <- unique(dict.df$ID_1[dict.df$ID_0 == id0[3]])
dict.df %>% filter(ID_1 == id1[7]) %>% 
  group_by(ID_2, FoodName_2) %>%  distinct(ID_2, FoodName_2) %>% knitr::kable()

dict.df %>% 
  group_by(ID_1, FoodName_1, ID_2, FoodName_2) %>%  distinct(ID_2, FoodName_2) %>% count() %>% 
  arrange(desc(n))

subset(dict.df, ID_2 == "23530")

#Checking categories (ID_3) by ID_2
dict.df %>% 
  group_by(ID_2, FoodName_2, ID_3, FoodName_3) %>%  distinct(ID_3, FoodName_3) %>% count() %>% 
  arrange(desc(n))

subset(dict.df, ID_2 == "23530")

#Run this to over-write any new upgrades in adding new food dictionary codes
#in dictionary folder


#IMPORTANT: to keep all folder updated run this code!!
#Saving Food-Dictionary into all r-project that use it

#here we are checking where the previous version was stored:
#
rproject.path <- str_remove(getwd(), "\\/[:alpha:]{1,}$")

#knowing all the MAPS_Dictionary-Protocol.R

x  <- list.files(rproject.path, 
                 pattern = "MAPS_Dictionary-Protocol.R",
                 recursive = TRUE)
#
#
#
## find the files that you want
#dictionary.files <- list.files(rproject.path, 
#                               pattern = "MAPS_Dictionary_v2.5.csv",
#                               recursive = TRUE) 
#
## copy the files to the new folder
#file.copy("MAPS_Dictionary-Protocol.R", file.path(rproject.path,
#                                                  x[2]), overwrite = TRUE)
#
##a loop to check that paths are created
#for(i in 1:length(x)){
#  
#  file.copy("MAPS_Dictionary-Protocol.R", file.path(rproject.path,
#            x[i]))
#  print(i)
#  
#}
#
##Storing the files paths for the latest version
#dictionary.files <- list.files(rproject.path, 
#                               pattern = "MAPS_Dictionary_v2.5.csv",
#                               recursive = TRUE) 
#
##a loop to check that paths are created
#for(i in 1:length(dictionary.files)){
#
#file.path(rproject.path,
#          str_replace(dictionary.files[i],
#          "MAPS_Dictionary_v2.5.csv", "MAPS_Dictionary_v2.6.csv"))
#print(i)
#
#}
#
#
##The loop that actually create the new file in each folder. 
#
#for(i in 1:length(dictionary.files)){
#
#  dictionary.df %>% 
#  write.csv(file.path(rproject.path,
#                str_replace(dictionary.files[i],
#          "MAPS_Dictionary_v2.5.csv", "MAPS_Dictionary_v2.6.csv")),
#   row.names = F)
#
#  print(i)
#}
#
#
##IMPORTANT: This file needs to be updated in Teams!!
##This will only be necessary when a new release is made. 
#
##Saving a copy of the master file for MAPS
#
#dictionary.df  %>% filter(str_detect(ID_3, "\\b")) %>% 
#  select(ID_0:FoodName_1, ID_3, FoodName_3) %>% 
#  rename(
#    food_group_id = "ID_0",
#    food_group_name = "FoodName_0",
#    food_subgroup_id = "ID_1",
#    food_subgroup_name = "FoodName_1",
#    food_genus_id = "ID_3",
#    food_genus_name = "FoodName_3") %>% 
#  write.csv(here::here('output',
#                       'MAPS_Dictionary_master-file_v2.6.csv'), row.names = F)
#