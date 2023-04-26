library(RODBC)
library(tidyverse)
source(here::here("functions.R"))


# Data Import ----

#legacy_database <- odbcConnectAccess2007("../../FAO/UoN-FAO/US19/SR-Leg_DB/SR_Legacy.accdb") #provides a link to the access database. Due to the database being too big to store on git, different locations have been used depending on who is running the code
legacy_database <- odbcConnectAccess2007(here::here("US19", "SR-LEG_DB", "SR_Legacy.accdb")) #alternate database location


legacy_database_tables <- sqlTables(legacy_database) #Creates a list of the tables in the database
legacy_database_table_names <- legacy_database_tables[,3][legacy_database_tables[4] == "TABLE"] #Creates a list of the data tables from the database (removes system tables)

for(i in c(2,4,5,9,10)){ #Loops through each data table that is of use
  temp_table <- sqlFetch(legacy_database, legacy_database_table_names[i]) #assigns each useful data table from the database a temporary R dataframe
  assign(paste0("LegTable_", i), temp_table) #renames the temporary table to save them out. The guide for the tables is below, as well as if they're used. 
}

#Table 1 is a list of source material.
#Table 2 - USED - links the nutrients for different food items to the source material
#Table 3 is an explanation table explaining where the items are derived from
#Table 4 - USED - lists food groups
#Table 5 - USED - is a food item table - the name (long and short), group ID, Food ID etc.
#NOTE: Table 5 appears to have an issue in the short description, no spaces after commas
#Table 6 is a list of addendums - clarifications on the types of nutrients added etc
#Table 7 is a key explaining processing types and product categories
#Table 8 links those processing factors and food categories to food item ID's
#Table 9 - USED - links food ID's to individual nutrients, with some useful metadata (number of studies/data points, errors etc)
#Table 10 - USED - A legend for the different Nutrients used in Table 9
#Table 11 is a legend for the nutrient source calculations - done for each nutrient, not each food item
#Table 12 is a unit key - translates the units each food item is found in to grams (cups, sticks etc to grams)



# Linking Nutrient codes to nutrient values ----

nutrient_code_list <- LegTable_10$Nutr_no #Creates a list of nutrient codes
sort(nutrient_code_list) #Sorts the list
Composite_Table <- LegTable_5 #Creates Composite_Table out of LegTable_5 - this will be the base table that is expanded on

for(i in 1:length(nutrient_code_list)){ #Loops through all the nutrients in the nutrient list
  temp_table <- LegTable_9 %>% filter(Nutr_No == nutrient_code_list[i]) #creates a temporary table consisting of all entries of LegTable_9 with the current Nutrient Number in the list
  #print(nutrient_code_list[i]) #OPTIONAL: Used to check the nutrient being examined
  #assign(paste0("NutrientTable_", nutrient_code_list[i]), temp_table)
  Composite_Table$NewCol <- NA #Creates a new column in the composite table
  for(c in 1:nrow(temp_table)){ #Loops through each row of the temp table
    food_ID <- temp_table[c,1] #checks the entry for the first column of that row, creates a variable for it - this is the food ID of the entry in question
    Composite_Table$NewCol[Composite_Table[,1]==food_ID] <- temp_table[c,3] #Assigns the nutrient data value to the new column for that specific food item to the New column
  }
  nutrient_row <- LegTable_10 %>% filter(Nutr_no == nutrient_code_list[i]) #Finds the nutrient details for that nutrient number
  names(Composite_Table)[names(Composite_Table) == 'NewCol'] <- paste0(nutrient_row[4], " (", nutrient_row[2], ")") #This part compresses headers that overrun multiple rows into one, incorporating that nutrient name
}



# Linking Food Group to Food Group Code ----

Composite_Table$foodgroup <- NA #Creates a new column and fills it with NA
for(u in 1:nrow(LegTable_4)){ #Loops through all rows of LegTable_4
  Composite_Table$foodgroup[Composite_Table$FdGrp_Cd == LegTable_4[u,1]] <- LegTable_4[u,2] #In Composite_Table, where the FdGrp_Cd equals an entry in the first column of LegTable_4, foodgroup for those rows is set to equal column 2 of LegTable_4, the description
}
Composite_Table <- Composite_Table %>% #moves foodgroup to after FdGrp_Cd, then deletes FdGrp_Cd
  relocate(foodgroup, .after = FdGrp_Cd) %>% mutate(
    FdGrp_Cd = NULL
  )



# Create nutrient data source list ----

Composite_Table$nutrient_data_source <- NA #Creates the new nutrient_dat_source column, fills it with NA
food_item_list_LT2 <- unique(LegTable_2$NDB_No) #Creates a list of the unique food IDs in Legacy Table 2
for(fooditem in food_item_list_LT2){ #iterates through every food item code on the list
  temp_table <- LegTable_2 %>% filter(NDB_No == fooditem) #Creates a temporary table of all rows in LegTable_2 related to the food item in question
  fooditem_sources <- unique(temp_table$DataSrc_ID) #Creates a list of unique data source ID's from that filtered table
  Composite_Table$nutrient_data_source[Composite_Table$NDB_No == fooditem] <- paste(fooditem_sources, collapse = ", ") #In the main table, where the NDB_No matches the fooditem in the list, the nutrient_data_source column is populated with this list
}



# Create FCT column ----

Composite_Table$source_fct <- "US19" #Creates the source_fct column, populates it with "US19"

#glimpse(Composite_Table) #Optional - used to glimpse the dataframe as it stands



# Create Edible column column ----

Composite_Table$EDIBLE <- 1-(Composite_Table$Refuse/100) #Edible fraction is calculated from the Refuse value



# Output Table renaming & tidying ----
# TO - DO: VITA is provided as IU - need to double check 
Output_table <- Composite_Table %>%
  select(-c("Com_Name", "ManufacName", "Ref_Desc")) %>% #These three columns are removed
  rename( #Some columns are renamed - e.g. NDB_No is renamed to fdc_id
    fdc_id = "NDB_No",
    food_desc = "Long_Desc",
    food_group = "foodgroup",
    scientific_name = "Sci_Name",
    Edible_factor_in_FCT = "EDIBLE",
    #  XN = "N_FActor", # until confirm w/ FAO
    #food_genus_id = "ID_3",
    #food_genus_description = "FoodName_3",
    #food_group = "FoodName_0",
    #food_subgroup = "FoodName_1",
    #food_genus_confidence = "confidence",
    WATERg = "Water (g)",
    ENERCkcal = "Energy (kcal)",
    ENERCkJ = "Energy (kJ)",
    PROCNTg = "Protein (g)",
    FATg = "Total lipid (fat) (g)", #as per FAO
    FASATg = "Fatty acids, total saturated (g)", 
    FAMSg = "Fatty acids, total monounsaturated (g)", 
    FAPUg = "Fatty acids, total polyunsaturated (g)", 
    CHOLEmg = "Cholesterol (mg)",
    CHOCDFg = "Carbohydrate, by difference (g)", 
    FIBTGg = "Fiber, total dietary (g)", 
    ALCg = "Alcohol, ethyl (g)", 
    ASHg = "Ash (g)",
    CAmg = "Calcium, Ca (mg)", 
    FEmg = "Iron, Fe (mg)",
    MGmg = "Magnesium, Mg (mg)",
    Pmg = "Phosphorus, P (mg)",
    Kmg = "Potassium, K (mg)",
    NAmg = "Sodium, Na (mg)", 
    ZNmg = "Zinc, Zn (mg)",
    CUmg = "Copper, Cu (mg)", 
    MNmg = "Manganese, Mn (mg)",
    SEmcg = "Selenium, Se (µg)",
    VITA_RAEmcg = "Vitamin A, RAE (µg)", 
    RETOLmcg = "Retinol (µg)", 
    CARTAmcg = "Carotene, alpha (µg)",
    CARTBmcg = "Carotene, beta (µg)", 
    CRYPXBmcg = "Cryptoxanthin, beta (µg)", 
    TOCPHAmg = "Vitamin E (alpha-tocopherol) (mg)",
    TOCPHBmg = "Tocopherol, beta (mg)",
    TOCPHGmg = "Tocopherol, gamma (mg)", 
    TOCPHDmg = "Tocopherol, delta (mg)", 
    TOCTRAmg = "Tocotrienol, alpha (mg)", 
    TOCTRBmg = "Tocotrienol, beta (mg)", 
    TOCTRGmg = "Tocotrienol, gamma (mg)",
    # TOCTRDmg ="Tocotrienol, delta (mg)", #to be confirmed w/ FAO
    VITCmg = "Vitamin C, total ascorbic acid (mg)",
    VITEmg = "Vitamin E (alpha-tocopherol) (mg)", 
    ERGCALmcg = "Vitamin D2 (ergocalciferol) (µg)",
    CHOCALmcg = "Vitamin D3 (cholecalciferol) (µg)",
    VITDmcg = "Vitamin D (D2 + D3) (µg)",
    THIAmg = "Thiamin (mg)",
    RIBFmg = "Riboflavin (mg)", 
    NIAmg = "Niacin (mg)", 
    TRPg = "Tryptophan (g)", 
    VITB6Amg = "Vitamin B-6 (mg)",
    #phyticacid_in_mg = "PHYTCPPD"), - not present
    FOLmcg = "Folate, total (µg)", 
    FOLACmcg = "Folic acid (µg)", 
    FOLFDmcg = "Folate, food (µg)",
    FOLDFEmcg = "Folate, DFE (µg)", 
    VITB12mcg = "Vitamin B-12 (µg)",
    SUGARg = "Sugars, total (g)",
    F22D6N3g = "22:6 n-3 (DHA) (g)", 
    F20D5N3g = "20:5 n-3 (EPA) (g)") %>%
  mutate( TRPmg =  TRPg*1000, #convert TRP from g to mg
          comment = NA) %>%   
  CARTBEQmcg_std_creator() %>%
  #  rename(CARTBEQmcg = "CARTBEQmcg_std") %>%   #Changing name of re-calculated variable for making VITA f(x) to work)
  VITAmcg_std_creator() %>%   #Re-calcuating VITAmcg
  relocate(food_group, .after = food_desc) %>% #Some columns are relocated for easier reading
  relocate(source_fct, .after = food_group) %>%
  relocate(nutrient_data_source, .after = source_fct) %>%
  relocate(WATERg, .after = CHO_Factor) %>%
  relocate(ENERCkcal, .after = WATERg) %>%
  relocate(ENERCkJ, .after = ENERCkcal) %>%
  relocate(Edible_factor_in_FCT, .after = Refuse)

#Re-calcuating VITAmcg from Vit.A (IU)
#Checking if RETOLmcg or CARTBmcg are available
which(is.na(Output_table$VITAmcg_std) & !is.na(Output_table$VITAmcg_std))
which(is.na(Output_table$VITAmcg_std))

#Checking where VITAmcg could be re-caluclated form VIT A (IU)
n <- which(is.na(Output_table$VITAmcg_std) & !is.na(Output_table$`Vitamin A, IU (IU)`))

#Recalculating - FAO/ INFOODS Guidelines for Converting Units, Denominators and Expressions Version 1.0
#suggested converting when no other info is available and "assuming" all VITA is retinol.
#Creating VITAmcg so VITAmcg_std is not over-written by the use of VITAmcg_std_creator() in compilation
Output_table$VITAmcg <-  NA
Output_table[n, "VITAmcg"] <- Output_table[n, "Vitamin A, IU (IU)"]*0.3

#Adding info into comment variable
Output_table$comment <-  NA
Output_table[n, "comment"] <- "VITAmcg was recalculated from Vitamin A, IU (IU)*03. See documentation for more information"

#Optional - check data before saving
glimpse(Output_table)

# Data Output ----

write.csv(Output_table, file = here::here("Output", "US19_FCT_FAO_Tags.csv"), row.names = FALSE)  #Saves the newly-created data table to the Output folder 
rm(list = ls())  #Removes all the environment variables - tidies up RStudio 