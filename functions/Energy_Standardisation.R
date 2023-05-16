#---
#title: Energy Standardisation
#author: Thomas Codd
#Github: https://github.com/TomCodd/Nutrition_Functions
#---

library(tidyverse)
library (docstring)

ENERCKj_standardised <- function(PROT, FAT, CHOAVLDF, FBGT, ALC = 0){
  
  #' A function used to calculate standardized Energy values in kJ
  #' 
  #' @description This function works as a basic calculator - The values for 
  #' Total Protein (PROT), Total Fat (FAT), Available Carbohydrate (CHOAVLDF), 
  #' Fibre, Total Dietary (FBTG) and Alcohol (ALC). Alcohol is optional, whereas
  #' the other inputs are required - if Alcohol is missing it is assumed to be
  #' 0.
  #' 
  #' @param PROT Required - The Total Protein value (in grams) for the food 
  #' item being examined.
  #' @param FAT Required - The Total Fat value (in grams) for the food item 
  #' being examined.
  #' @param CHOAVLDF Required - The Total Available Carbohydrate value (in 
  #' grams) for the food item being examined.
  #' @param FBGT Required - The Total Dietary Fibre value (in grams) for the 
  #' food item being examined.
  #' @param ALC Optional - The Total Alcohol value (in grams) for the food item 
  #' being examined.
  #' 
  #' @return The calculated Energy value in kJ.
  #' 
  #' @examples Three examples will be covered - two variants for a one-off 
  #' calculation, and to create a column with the calculated results.
  #' 
  #' Single calculation:
  #' 
  #' Bread, wheat, white, unfortified
  #' 
  #' Protein_value <- 7.5
  #' Fat_value <- 1.3
  #' Carb_value <- 50.5
  #' Fibre_value <- 2.9
  #' Alcohol_value <- 0
  #' 
  #' standardised_kJ <- ENERCKj_standardised(PROT = Protein_value, FAT = Fat_value, 
  #' CHOAVLDF = Carb_value, FBTG = Fibre_value, ALC = Alcohol_value)
  #' 
  #' alternatively:
  #' 
  #' standardised_kJ <- ENERCKj_standardised(PROT = 7.5, FAT = 1.3, 
  #' CHOAVLDF = 50.5, FBTG = 2.9, ALC = 0)
  #' 
  #' data.frame calculation:
  #' 
  #' First, an example dataframe is outlined and created - 
  #' 
  #' test_df_WAFCT2019 <- data.frame(
  #' c("Bread, wheat, white, unfortified",
  #' "Beer, European (4.6% v/v alcohol)",
  #' "Maize, yellow, meal, whole grains, unfortified",
  #' "Sweet potato, yellow flesh, raw",
  #' "Cassava, tuber, white flesh, raw"),
  #' c(7.5, 0.3, 9.4, 1.5, 1.3),
  #' c(1.3, 0, 3.7, 0.2, 0.3),
  #' c(50.5, 3.7, 65.2, 25.5, 31.6),
  #' c(2.9, 0, 9.4, 3, 3.7),
  #' c(0, 3.6, 0, NA, 0))
  #' 
  #' Then, the columns are renamed:
  #' 
  #' colnames(test_df_WAFCT2019) <- c("food_name", "protein", "fat", "carbs",
  #' "fb", "alcohol")
  #' 
  #' Once renamed, the function is applied. the assigned output is a new column 
  #' in the data.frame, and the inputs are the different columns detailing the 
  #' relevant food nutrient values.
  #' 
  #' test_df_WAFCT2019$ENERCKj_stnd <- ENERCKj_standardised(
  #'          test_df_WAFCT2019$protein, 
  #'          test_df_WAFCT2019$fat, 
  #'          test_df_WAFCT2019$carbs,
  #'          test_df_WAFCT2019$fb,
  #'          test_df_WAFCT2019$alcohol)

  stopifnot("No Protein value assigned. Please assign a PROT value." = !is.null(PROT))
  stopifnot("No Protein value assigned. Please assign a PROT value." = !is.na(PROT))
  stopifnot("No Fat value assigned. Please assign a FAT value." = !is.null(FAT))
  stopifnot("No Fat value assigned. Please assign a FAT value." = !is.na(FAT))
  stopifnot("No Available Carbohydrate value assigned. Please assign a CHOAVLDF value." = !is.null(CHOAVLDF))
  stopifnot("No Available Carbohydrate value assigned. Please assign a CHOAVLDF value." = !is.na(CHOAVLDF))
  stopifnot("No Dietary Fibre value assigned. Please assign an FBGT value." = !is.null(FBGT))
  stopifnot("No Dietary Fibre value assigned. Please assign an FBGT value." = !is.na(FBGT))
  
  if (length(ALC) > 1){
    ALC <- ALC %>% replace_na(0)
  } else {
  if (is.na(ALC)){ALC <- 0}
  }

  ENERCKj_std <- PROT*17 + FAT*37 + CHOAVLDF*17 + FBGT*8 + ALC*29
  
  if (length(PROT)==1){
    message(paste0("Total Protein value (g): ", PROT))
    message(paste0("Total Fat value (g): ", FAT))
    message(paste0("Total Available Carbohydrate value (g): ", CHOAVLDF))
    message(paste0("Total Dietary Fibre value (g): ", FBGT))
    message(paste0("Alcohol value (g): ", ALC))
    message(paste0("----------------------------------------------"))
    message(paste0("Energy kJ Standardised value: ", ENERCKj_std))
  }
  
  return(ENERCKj_std)
}




ENERCKcal_standardised <- function(PROT, FAT, CHOAVLDF, FBGT, ALC = 0){
  
  #' A function used to calculate standardized Energy values in kcal
  #' 
  #' @description This function works as a basic calculator - The values for 
  #' Total Protein (PROT), Total Fat (FAT), Available Carbohydrate (CHOAVLDF), 
  #' Fibre, Total Dietary (FBTG) and Alcohol (ALC). Alcohol is optional, whereas
  #' the other inputs are required - if Alcohol is missing it is assumed to be
  #' 0.
  #' 
  #' @param PROT Required - The Total Protein value (in grams) for the food 
  #' item being examined.
  #' @param FAT Required - The Total Fat value (in grams) for the food item 
  #' being examined.
  #' @param CHOAVLDF Required - The Total Available Carbohydrate value (in 
  #' grams) for the food item being examined.
  #' @param FBGT Required - The Total Dietary Fibre value (in grams) for the 
  #' food item being examined.
  #' @param ALC Optional - The Total Alcohol value (in grams) for the food item 
  #' being examined.
  #' 
  #' @return The calculated Energy value in kcal.
  #' 
  #' @examples Three examples will be covered - two variants for a one-off 
  #' calculation, and to create a column with the calculated results.
  #' 
  #' Single calculation:
  #' 
  #' Bread, wheat, white, unfortified
  #' 
  #' Protein_value <- 7.5
  #' Fat_value <- 1.3
  #' Carb_value <- 50.5
  #' Fibre_value <- 2.9
  #' Alcohol_value <- 0
  #' 
  #' standardised_kcal <- ENERCKcal_standardised(PROT = Protein_value, FAT = Fat_value, 
  #' CHOAVLDF = Carb_value, FBTG = Fibre_value, ALC = Alcohol_value)
  #' 
  #' alternatively:
  #' 
  #' standardised_kcal <- ENERCKcal_standardised(PROT = 7.5, FAT = 1.3, 
  #' CHOAVLDF = 50.5, FBTG = 2.9, ALC = 0)
  #' 
  #' data.frame calculation:
  #' 
  #' First, an example dataframe is outlined and created - 
  #' 
  #' test_df_WAFCT2019 <- data.frame(
  #' c("Bread, wheat, white, unfortified",
  #' "Beer, European (4.6% v/v alcohol)",
  #' "Maize, yellow, meal, whole grains, unfortified",
  #' "Sweet potato, yellow flesh, raw",
  #' "Cassava, tuber, white flesh, raw"),
  #' c(7.5, 0.3, 9.4, 1.5, 1.3),
  #' c(1.3, 0, 3.7, 0.2, 0.3),
  #' c(50.5, 3.7, 65.2, 25.5, 31.6),
  #' c(2.9, 0, 9.4, 3, 3.7),
  #' c(0, 3.6, 0, NA, 0))
  #' 
  #' Then, the columns are renamed:
  #' 
  #' colnames(test_df_WAFCT2019) <- c("food_name", "protein", "fat", "carbs",
  #' "fb", "alcohol")
  #' 
  #' Once renamed, the function is applied. the assigned output is a new column 
  #' in the data.frame, and the inputs are the different columns detailing the 
  #' relevant food nutrient values.
  #' 
  #' test_df_WAFCT2019$ENERCKcal_stnd <- ENERCKcal_standardised(
  #'          test_df_WAFCT2019$protein, 
  #'          test_df_WAFCT2019$fat, 
  #'          test_df_WAFCT2019$carbs,
  #'          test_df_WAFCT2019$fb,
  #'          test_df_WAFCT2019$alcohol)
  
  stopifnot("No Protein value assigned. Please assign a PROT value." = !is.null(PROT))
  stopifnot("No Protein value assigned. Please assign a PROT value." = !is.na(PROT))
  stopifnot("No Fat value assigned. Please assign a FAT value." = !is.null(FAT))
  stopifnot("No Fat value assigned. Please assign a FAT value." = !is.na(FAT))
  stopifnot("No Available Carbohydrate value assigned. Please assign a CHOAVLDF value." = !is.null(CHOAVLDF))
  stopifnot("No Available Carbohydrate value assigned. Please assign a CHOAVLDF value." = !is.na(CHOAVLDF))
  stopifnot("No Dietary Fibre value assigned. Please assign an FBGT value." = !is.null(FBGT))
  stopifnot("No Dietary Fibre value assigned. Please assign an FBGT value." = !is.na(FBGT))
  
  if (length(ALC) > 1){
    ALC <- ALC %>% replace_na(0)
  } else {
    if (is.na(ALC)){ALC <- 0}
  }
  
  ENERCKcal_std <- PROT*4 + FAT*9 + CHOAVLDF*4 + FBGT*2 + ALC*7
  
  if (length(PROT)==1){
    message(paste0("Total Protein value (g): ", PROT))
    message(paste0("Total Fat value (g): ", FAT))
    message(paste0("Total Available Carbohydrate value (g): ", CHOAVLDF))
    message(paste0("Total Dietary Fibre value (g): ", FBGT))
    message(paste0("Alcohol value (g): ", ALC))
    message(paste0("----------------------------------------------"))
    message(paste0("Energy kcal Standardised value: ", ENERCKcal_std))
  }
  
  return(ENERCKcal_std)
}

test_df_WAFCT2019 <- data.frame(c("Bread, wheat, white, unfortified",
                                   "Beer, European (4.6% v/v alcohol)",
                                   "Maize, yellow, meal, whole grains, unfortified",
                                   "Sweet potato, yellow flesh, raw",
                                   "Cassava, tuber, white flesh, raw"),
                       c(7.5, 0.3, 9.4, 1.5, 1.3),
                       c(1.3, 0, 3.7, 0.2, 0.3),
                       c(50.5, 3.7, 65.2, 25.5, 31.6),
                       c(2.9, 0, 9.4, 3, 3.7),
                       c(0, 3.6, 0, NA, 0))

colnames(test_df_WAFCT2019) <- c("food_name", "protein", "fat", "carbs", "fb", "alcohol")

test_df_WAFCT2019$ENERCKj_standardised <- ENERCKj_standardised(test_df_WAFCT2019$protein, 
                                                     test_df_WAFCT2019$fat, 
                                                     test_df_WAFCT2019$carbs,
                                                     test_df_WAFCT2019$fb,
                                                     test_df_WAFCT2019$alcohol)


test_df_WAFCT2019$ENERCKcal_standardised <- ENERCKcal_standardised(test_df_WAFCT2019$protein, 
                                                     test_df_WAFCT2019$fat, 
                                                     test_df_WAFCT2019$carbs,
                                                     test_df_WAFCT2019$fb,
                                                     test_df_WAFCT2019$alcohol)