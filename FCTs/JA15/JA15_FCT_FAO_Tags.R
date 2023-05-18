library(tidyverse)
source("functions.R")


# Data Import ----

JA15_Raw_FCT <- readxl::read_excel(here::here("FCTS",  'JA15', "Japan_FCT.xlsx"), sheet = 1) %>%
  slice(-c(1:3)) #Removes useless rows 



# Column Renaming ----

JA15_Raw_FCT[JA15_Raw_FCT == "%"] <- "pc"
JA15_Raw_FCT[JA15_Raw_FCT == "kcal/100 g"] <- "kcal" #This codeblock replaces the units with more standard SI units before column names are set with them
JA15_Raw_FCT[JA15_Raw_FCT == "kJ/100 g"] <- "kJ"
JA15_Raw_FCT[JA15_Raw_FCT == "g/100 g"] <- "g"
JA15_Raw_FCT[JA15_Raw_FCT == "mg/100 g"] <- "mg"
#JA15_Raw_FCT[JA15_Raw_FCT == "?g/100 g"] <- "mcg"
JA15_Raw_FCT[JA15_Raw_FCT == "µg/100 g"] <- "mcg"

for (i in 1:ncol(JA15_Raw_FCT)){ #This loop goes through all the FAO tags, and fills in with the other title for that column if the FAO tag is missing.
  if((paste0(JA15_Raw_FCT[2,i]) == paste0(JA15_Raw_FCT[2,67]))){ #for whatever reason I can't replicate the cell contents of what they're using to signify no tag, so had to use JA15_Raw_FCT[2,67] as an example to match against
    JA15_Raw_FCT[2,i] <- paste0(JA15_Raw_FCT[1,i], "_")
  }
}

JA15_Raw_FCT[2,6] <- "ENERC" #Manually Renames this entry to the FAO tag name

colnames(JA15_Raw_FCT) <- paste0(JA15_Raw_FCT[2,], JA15_Raw_FCT[3,])#Sets the general rule for the column heading names (the FAO tag and then the units)
colnames(JA15_Raw_FCT)[c(1:4)] <- c("Food Group", "Item Number", "Index Number", "Food Description") #Manually renames some entries the general rule doesn't work for
JA15_FCT_int <- JA15_Raw_FCT

JA15_FCT_int <- JA15_FCT_int[-c(1:3),-68] #Removes the rows that now form the column headers, and the final column (the remarks column originally)



# Setting food group ----

Food_Group_Names <- c("Cereals", "Potatoes and starches", "Sugars and sweeteners", "Pulses", "Nuts and seeds", "Vegetables", "Fruits", "Mushrooms", "Algae", "Fish, mollusks and crustaceans",
                      "Meat", "Eggs", "Milk and milk products", "Fats and oils", "Confectionaries", "Beverages", "Seasonings and spices", "Prepared foods") #Lists all the food groups from the PDF
JA15_FCT_int$`Food Group` <- as.integer(JA15_FCT_int$`Food Group`) #Sets the food group codes as integers, prep for the next step
for (i in 1:length(Food_Group_Names)){ #iterates over the food group codes, and replaces them with the food group name for ease of understanding
  JA15_FCT_int$`Food Group`[JA15_FCT_int$`Food Group` == i] <- Food_Group_Names[i]
}



# Data import and column renaming - subtables ----

#The following blocks of code do the same rough functions as the first data import and renaming blocks, but with the other files.


JA15_Raw_FCT_Amino_Acids <- readxl::read_excel(here::here("FCTs", 'JA15', "JA15_FCT_Amino_Acids.xlsx"), sheet = 1) %>%
  slice(-c(1:3))
JA15_FCT_Amino_Acids_int <- JA15_Raw_FCT_Amino_Acids[-c(1,3:7, 32)] #This is to remove rows that are already covered by the main FCT, to stop doubling up once joined


JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "%"] <- "pc"
JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "kcal/100 g"] <- "kcal" #This codeblock replaces the units with more standard SI units before column names are set with them
JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "kJ/100 g"] <- "kJ"
JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "g/100 g"] <- "g"
JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "mg/100 g"] <- "mg"
JA15_FCT_Amino_Acids_int[JA15_FCT_Amino_Acids_int == "?g/100 g"] <- "mcg"

for (i in 1:ncol(JA15_FCT_Amino_Acids_int)){ #This function goes through all the FAO tags, and fills in with the other title for that column if the FAO tag is missing.
  if((paste0(JA15_FCT_Amino_Acids_int[2,i]) == paste0(JA15_FCT_Amino_Acids_int[2,25]))){ #for whatever reason I can't replicate the cell contents of what they're using to signify no tag, so had to use JA15_FCT_Amino_Acids_int[2,67] as an example to match against
    JA15_FCT_Amino_Acids_int[2,i] <- paste0(JA15_FCT_Amino_Acids_int[1,i], "_")
  }
}

colnames(JA15_FCT_Amino_Acids_int) <- paste0(JA15_FCT_Amino_Acids_int[2,], JA15_FCT_Amino_Acids_int[3,])#Sets the general rule for the column heading names (the FAO tag and then the units)

JA15_FCT_Amino_Acids_int <- JA15_FCT_Amino_Acids_int %>%
  rename("Item Number" = colnames(JA15_FCT_Amino_Acids_int)[1]) #Manually renames one column the general rule doesn't work for
JA15_FCT_Amino_Acids_int <- JA15_FCT_Amino_Acids_int[-c(1:3),] #removes the rows used to generate the new column names



JA15_Raw_FCT_CPOA <- readxl::read_excel(here::here("FCTs", 'JA15', "JA15_FCT_Carbs_Polyols_Organic_Acids.xlsx"), sheet = 1) %>%
  slice(-c(1:3))
JA15_FCT_CPOA_int <- JA15_Raw_FCT_CPOA[-c(1,3:6, 18)] #This is to remove rows that are already covered by the main FCT, to stop doubling up once joined


JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "%"] <- "pc"
JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "kcal/100 g"] <- "kcal" #This codeblock replaces the units with more standard SI units before column names are set with them
JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "kJ/100 g"] <- "kJ"
JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "g/100 g"] <- "g"
JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "mg/100 g"] <- "mg"
JA15_FCT_CPOA_int[JA15_FCT_CPOA_int == "?g/100 g"] <- "mcg"

colnames(JA15_FCT_CPOA_int) <- paste0(JA15_FCT_CPOA_int[2,], JA15_FCT_CPOA_int[3,])#Sets the general rule for the column heading names (the FAO tag and then the units)


colnames(JA15_FCT_CPOA_int)[1] <- c("Item Number")  #Manually renames one column the general rule doesn't work for
JA15_FCT_CPOA_int <- JA15_FCT_CPOA_int[-c(1:3),]  #removes the rows used to generate the new column names




JA15_Raw_FCT_Fatty_Acids <- readxl::read_excel(here::here("FCTs", 'JA15', "JA15_FCT_Fatty_Acids.xlsx"), sheet = 1) %>%
  slice(-c(1:3))
JA15_FCT_Fatty_Acids_int <- JA15_Raw_FCT_Fatty_Acids[-c(1,3:7, 9:11, 32)] #This is to remove rows that are already covered by the main FCT, to stop doubling up once joined

JA15_FCT_Fatty_Acids_int[2,31] <- "F22D1" #Typo in the original - description said F22D1, was accidentally put as F20D1, like its neighbour

JA15_FCT_Fatty_Acids_int[] <- lapply(JA15_FCT_Fatty_Acids_int, function(x) gsub("\\*|\\(|\\)", "", x))

JA15_FCT_Fatty_Acids_int[JA15_FCT_Fatty_Acids_int == "%"] <- "pc" #This codeblock replaces the units with more standard SI units before column names are set with them
JA15_FCT_Fatty_Acids_int[JA15_FCT_Fatty_Acids_int == "g/100 g"] <- "g"
JA15_FCT_Fatty_Acids_int[JA15_FCT_Fatty_Acids_int == "mg/100 g"] <- "g" #part 1 of the conversion to grams
JA15_FCT_Fatty_Acids_int[JA15_FCT_Fatty_Acids_int == "Tr"] <- "0"
JA15_FCT_Fatty_Acids_int[JA15_FCT_Fatty_Acids_int == "-"] <- NA

colnames(JA15_FCT_Fatty_Acids_int) <- paste0(JA15_FCT_Fatty_Acids_int[2,], JA15_FCT_Fatty_Acids_int[3,])#Sets the general rule for the column heading names (the FAO tag and then the units)

JA15_FCT_Fatty_Acids_int <- JA15_FCT_Fatty_Acids_int %>%
  rename("Item Number" = colnames(JA15_FCT_Fatty_Acids_int)[1]) #renames the column the general rule doesn't work for
JA15_FCT_Fatty_Acids_int <- JA15_FCT_Fatty_Acids_int[-c(1:3),]  #removes the rows used to generate the new column names


JA15_FCT_Fatty_Acids_int[, 5:54] <- lapply(5:54, function(x) as.numeric(JA15_FCT_Fatty_Acids_int[[x]])) #part 2 of the conversion to grams - all relevant columns converted to numeric (the only columns of this class)

JA15_FCT_Fatty_Acids_int <- mutate_if(JA15_FCT_Fatty_Acids_int, is.numeric, ~ . /1000) #part 3 of the conversion to grams - all numeric columns divided by 1000


# Merging tables ----

Output_table <- JA15_FCT_int #This batch of code sets the output to be a copy of the main FCT, then adds on the other FCT tables to them.
Output_table <- left_join(Output_table, JA15_FCT_Amino_Acids_int, by = "Item Number")
Output_table <- left_join(Output_table, JA15_FCT_CPOA_int, by = "Item Number")
Output_table <- left_join(Output_table, JA15_FCT_Fatty_Acids_int, by = "Item Number")



# Data tidying, column creation, moving and renaming ----

Output_table[Output_table == "-"] <- NA #Sets all missing entries to NA.
#Output_table[Output_table == "Tr"] <- "0" #Sets all "Tr" entries to 0

Output_table[,c(5:156)] <- apply(Output_table[,c(5:156)], 2, TraceToZero)

Output_table[] <- lapply(Output_table, function(x) gsub("\\*|\\(|\\)", "", x)) # removes special characters from the table contents

Output_table$source_fct <- "JA15" #creates and populates the "source_fct" column 
Output_table$EDIBLEpc <- (100-as.integer(Output_table$REFUSEpc))/100 #calculates the Edible percentage column (EDIBLEpc)
Output_table$nutrient_data_source <- "None listed" #creates and populates the data source column

names(Output_table)

Output_table <- Output_table %>%
  relocate(source_fct, .after = `Food Description`) %>% #moves columns to more relevant locations
  relocate(EDIBLEpc, .after = REFUSEpc) %>% 
  relocate(nutrient_data_source, .after = source_fct) %>% 
  rename( #manually renames metadata columns and some other columns to the project naming structure
    fdc_id = "Item Number", 
    food_desc = "Food Description", 
    food_group = "Food Group",
    food_group_id = "Index Number",
    Edible_factor_in_FCT = "EDIBLEpc", 
    PROCNTg = "Protein, calculated from  reference nitrogen_g", 
    CHOCDFg ="Carbohydrate, total, calculated by difference_g",
    FATg = "Lipid_g",
    Edible_info = "NANA"
  )


# Data Output ----

write.csv(Output_table, file = here::here("FCTs", "JA15_FCT_FAO_Tags.csv"), row.names = FALSE) #Saves the newly-cleaned table to the Output folder 

#Run this to clean the environment
rm(list = ls())
