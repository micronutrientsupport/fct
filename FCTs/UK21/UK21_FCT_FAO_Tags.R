library(tidyverse)



# Data Import ----

excel_sheets <- readxl::excel_sheets(here::here('UK21', "McCance_Widdowsons_Composition_of_Foods_Integrated_Dataset_2021..xlsx")) #Creates a list of the excel sheets present in the document

for (i in 1:length(excel_sheets)){ #Loops through the length of the list created above
  newsheet <- readxl::read_excel(here::here('UK21', "McCance_Widdowsons_Composition_of_Foods_Integrated_Dataset_2021..xlsx"), sheet = i) #Creates an R object from the corresponding excel sheet
  assign(paste0(excel_sheets[i]), newsheet) #Assigns the new sheet the name of the excel sheet from the original document
}

colnames(`1.4 Inorganics`)[1] <- "Food Code" #This table was missing a column header, this reinserts what it should be



# Creating metadata table ----

header_data <- ""

for(i in 3:15){ #For loop iterating over only the relevant data tables, taking the first 2 rows and adding them to a larger table for metadata, and cleaning up the rest
  sliced_table_first2 <- get(excel_sheets[i]) %>% slice(1:2) #Cuts the first two rows out to process
  first2_transposed <- data.frame(t(sliced_table_first2)) #Transposes the first two rows
  first2_transposed$X0 <- row.names(first2_transposed) #Applies the row names to the X0 column for first2_transposed
  first2_transposed <- first2_transposed %>%
    relocate(X0) #Moves the X0 column to be the first column
  row.names(first2_transposed) <- NULL #Removes the row names
  for (i in 1:nrow(first2_transposed)){
    header_data <- rbind(header_data, first2_transposed[i,]) #Attaching each row to the header_data table
  }
}

header_data <- header_data[!duplicated(header_data$X0),] %>%
  slice(-1) #Removes the first row from header_data


# Merging tables ----

for (i in 3:15){ #Used to check for duplicate entries before a merge/left join, and correct them, before moving on to strip and append the datasets corectly.
  sliced_table_data <- get(excel_sheets[i]) %>% slice(-c(1,2)) #Removes the first two rows, which only contain overflow header info
  test_table <- table(sliced_table_data$`Food Code`) #test to check how often each item in the Food Code column appears
  repeated_codes <- names(test_table[test_table>1]) #any time a code appears more than once it appears here
  for (repeated_code in repeated_codes){ #This loop changes the repeated code to be [old_code]_x for example
    names_with_same_code <- sliced_table_data$`Food Name`[sliced_table_data[,1] == repeated_code] #Creates list of the food item names that have duplicate codes
    for (o in 1:length(names_with_same_code)){ #loops through the list of names with the same code
      repeated_name <- names_with_same_code[o] #assigns the name in question to a variable
      print(repeated_name) #prints the name
      sliced_table_data$`Food Code`[sliced_table_data$`Food Name` == repeated_name] <- paste0(repeated_code, "_", o) #For those food items, modifies their code to add their position in the duplicate list to the code
    }
  }
  
  colnames(sliced_table_data)[1] <- "Food Code" #Assigns the column name "Food Code" for the sliced_table_data, where it was missing
  if(i==3){ 
    Output_table <- sliced_table_data #If the third table is being examined, this sets the output table to that table
  } else{
    stripped_table_data <- sliced_table_data[,-c(2:7)] #Otherwise, the other tables are stripped of metadata
    Output_table <- left_join(Output_table, stripped_table_data, by.all = "Food Code") #And then joined to the newly formed output table by the Food Code
  }
  
}



# Manual renaming and tidying ----

uk21_colnames <- c("fdc_id", #Creates a list of column names following thew FAO tagnames guidelines
                   "food_desc",
                   "comments", 
                   "food_group", 
                   "previous_fcd_id",
                   "nutrient_data_source", 
                   "footnote",
                   "Edible_factor_in_FCT",
                   "specific_gravity",
                   "total_solids",
                   "XN",
                   "XFA", 
                   "WATERg",
                   "NTg", 
                   "PROCNTg",
                   "FAT_g",
                   "CHOAVLg",
                   "ENERCkcal", 
                   "ENERCkJ", 
                   "STARCHMg", 
                   "OLSACMg", 
                   "SUGARMg", 
                   "GLUSg", 
                   "GALSg", 
                   "FRUSg",
                   "SUCSMg", 
                   "MALSMg",
                   "LACSMg",
                   "ALCg_100mL",
                   "NSPg",
                   "FIBTGg", 
                   "FASATg_100gFAT", 
                   "FASATg",
                   "FAPUN6g_100gFAT", 
                   "FAPUN6g",
                   "FAPUN3g_100gFAT",
                   "FAPUN3g",
                   "cisFAMSg_100gFAT",
                   "cisFAMSg",
                   "FAMSg_100gFAT",
                   "FAMSg",
                   "cisFAPUg_100gFAT",
                   "cisFAPUg",
                   "FAPUg_100gFAT",
                   "FAPUg",
                   "exFASATg_100gFAT", 
                   "exFASATg",
                   "brFACIDg_100gFAT", 
                   "brFACIDg", 
                   "FATRNg_100gFAT",
                   "FATRNg",
                   "CHOLmg",
                   "NAmg", 
                   "Kmg", 
                   "CAmg",
                   "MGmg",
                   "Pmg",
                   "FEmg",
                   "CUmg",
                   "ZNmg",
                   "CLDmg",
                   "MNmg", 
                   "SEmcg",
                   "IDmcg", 
                   "RETOLmcg", 
                   "CARTBEQmcg",
                   "VITAmcg", 
                   "VITDmcg", 
                   "VITEmg", 
                   "VITKmcg",
                   "THIAmg", 
                   "RIBFmg",
                   "NIAmg", 
                   "NIATRPmg", 
                   "NIAEQmg", 
                   "VITB6_mg", 
                   "VITB12mcg",
                   "FOLmcg",
                   "PANTACmg", 
                   "BIOTmcg",
                   "VITCmg", 
                   "transRETOLmg",
                   "cisRETOLmg",
                   "dehydroRETOLmg",
                   "RETALDmcg",
                   "CARTAmcg",
                   "CARTBmcg",
                   "Cryptoxanthinsmcg",
                   "Luteinmcg",
                   "LYCOmcg",
                   "CHOCALOHmcg", 
                   "CHOCALmcg", 
                   "methylFOLmcg", 
                   "TOCPHAmg", 
                   "TOCPHBmg", 
                   "TOCPHDmg", 
                   "TOCPHGmg", 
                   "TOCTRAmg", 
                   "TOCTRGmg",
                   rep(NA, 188)) #The final 188 entries are set to NA (this mostly only covers the fatty acids)



header_data[4] <-  uk21_colnames #creates a fourth column, listing the FAO tagnames

header_data$V4[header_data$X1 == "FOD20:5cn3"] <- "F20D5N3g" #Renames certain fao tagnames in the header_data
header_data$V4[header_data$X1 == "FOD22:6cn3"] <- "F22D6N3g" #Renames certain fao tagnames in the header_data


fct_tagname <- header_data %>% mutate(V4 = ifelse(is.na(V4), 
                                                  X1, 
                                                  V4)) #Creates a list of fct_tagnames - if the V4 column of header_data is na then it gets the name from X1 column, otherwise V43 column


Output_table <- Output_table %>% rename_all(~pull(fct_tagname[4])) #Uses these fct_tagnames to rename the Output_table

Output_table$source_fct <- "UK21_FCT" #Creates and sets the source_fct column

Output_table <- Output_table %>%
  relocate(source_fct, .after = food_desc) #Moves the source_fct column to the start, with other metadata columns

#Check "N" vs "Tr"
#Changing "Tr" to zero

TraceToZero <- function(x){
  x <- gsub("Trace|trace|Tr|tr", "0", x)
  return(x) 
}

Output_table[Output_table == "N"] <- NA # Sets all N values, defined as "where a nutrient is present in significant quantities, but there is no reliable information on the amount", to NA

Output_table[,c(9:288)] <- apply(Output_table[,c(9:288)], 2, TraceToZero)


write.csv(Output_table, file = here::here("Output", "UK21_FCT_FAO_Tags.csv"),
          row.names = FALSE) #Saves the newly-created data table to the Output folder

#Run this to clean the environment
rm(list = ls())
