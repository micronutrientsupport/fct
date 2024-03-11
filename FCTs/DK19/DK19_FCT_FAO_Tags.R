
library(tidyverse)



# Data Import ----

DK19_Raw <- readxl::read_excel(here::here("FCTs",'DK19', "Frida20190802env3.xlsx"), sheet = 2) #Reads the excel document and assigns the relevant sheet to an R data frame



# Column Creation & Renaming ----

colnames(DK19_Raw) <- paste0(colnames(DK19_Raw), " (", DK19_Raw[1,], ")") #This part compresses headers that overrun multiple rows into one
colnames(DK19_Raw)[1:3] <- c("ID_Number", "Food_Group", "Food_Name") #And manually renames the metadata columns
Output_table <- DK19_Raw %>% slice(-1) #Before removing the row the column names had overrun on to

Output_table$source_fct <- "DK19" #Creates the source FCT column, and populates it with "DK19"
Output_table$nutrient_data_source <- "None listed" #Creates the nutrient data source column, and notifies no data sources are listed



# Column Relocating ----

Output_table <- Output_table %>%
  relocate(source_fct, .after = Food_Name) %>% #Moves the source_fct column to the metadata columns at the start of the table
  relocate(nutrient_data_source, .after = source_fct) #Moves the nutrient data source column to the metadata columns at the start of the table



# Column Manual Renaming ----

col_tagname <- c("fdc_id", "food_group", #List of FAO tagnames, in the same order as the corresponding columns
                 "food_desc", "source_fct",
                 "nutrient_data_source", "waste_per",
                 "ENERCkJ", "ENERCkcal",
                 "XN", "NTg",
                 "PROCNTg", NA, 
                 "CHOCDFg", "CHOAVLDFg", 
                 NA, NA, 
                 "FIBTGg", "FAT_g", 
                 NA, "ALC_g", 
                 "ASHg", NA, 
                 "WATERg", "VITA_RAEmcg",
                 "RETOLmcg", "CARTBmcg",
                 "VITDEQmcg", "CHOCALmcg",
                 "ERGCALmcg", "CHOCALOHmcg",
                 "ERGCALOHmcg", "VITEmg",
                 "TOCPHAmg", "TOCPHGmg",
                 "TOCPHDmg","TOCTRAmg",
                 NA, "THIAHCLmg",
                 "THIAmg", "RIBFmg",
                 "NIAEQmg",  "NIAmg",
                 "VITB6Cmg", NA, 
                 NA, "FOL_mcg",
                 NA, "VITB12mcg",
                 "VITCmg", "ASCLmg", 
                 "ASCDLmg", NA,
                 NA, "NAmg", 
                 "Kmg", "CAmg",
                 "MGmg", "Pmg",
                 "FEmg", "CUmg", 
                 "ZNmg", "IDmcg",
                 "MNmg", "CRmcg",
                 "SEmcg", "MOmcg", 
                 NA, NA, 
                 NA, NA, 
                 NA, NA,
                 NA, NA,
                 NA, NA, 
                 NA, NA,
                 NA, NA, 
                 NA, NA,  
                 NA, NA,  
                 NA, NA, 
                 NA, NA,  
                 NA, NA,  
                 NA, NA,  
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA,
                 NA, NA,
                 NA, NA, 
                 NA, NA,
                 NA, NA, 
                 NA, NA,  
                 NA, NA,  
                 NA, NA, 
                 NA, NA,  
                 NA, NA,  
                 NA, NA,  
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, "F20D5N3g", 
                 NA, "F22D6N3g", 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 NA, NA, 
                 "FASAT", "FAMSg",
                 "FAPUg", "FATRNg",
                 NA, NA,  
                 NA, "CHOLEmg",
                 NA, NA,
                 NA, NA,
                 NA, NA,
                 NA, NA,
                 "TRPmg", NA,
                 NA, NA,
                 NA, NA,
                 NA, NA,
                 NA, NA,
                 NA, "specific_gravity"
)

col_tagname[105] <- "SUGARg"

for (i in 1:length(col_tagname)){ #This for loop checks to see if the tagname list gives an NA - if not, then the column name is replaced with the tagname
  if(!is.na(col_tagname[i])){
    colnames(Output_table)[i] <- c(col_tagname[i])
  }
}



# Calculating Edible_factor_in_FCT ----

Output_table$Edible_factor_in_FCT <- (100-as.numeric(Output_table$waste_per))/100 #converts the waste percentage to an Edible fraction
Output_table <- Output_table %>%
  relocate(Edible_factor_in_FCT, .after = waste_per)



# Changing no value ("nv") to NA ----

Output_table[Output_table == "nv"] <- NA #Sets no value entries to NA



# Data Output ----

glimpse(Output_table)

write.csv(Output_table, file = here::here("FCTs", "DK19_FCT_FAO_Tags.csv"),
          row.names = FALSE) #Saves the newly-cleaned table to the Output folder 

#clean environment
rm(list = ls())