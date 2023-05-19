#---
#title: Add to Dictionary
#author: Thomas Codd
#Github: https://github.com/TomCodd
#---


library(tidyverse)

Find_ID_3 <- function(dictionary, ID_2){
  other_ID2s <- dictionary[dictionary$ID_2 == ID_2,] #narrows down the dictionary to all entries which have the same ID_2
  other_ID2s <- other_ID2s[!is.na(other_ID2s$ID_2),] #Removes NA's
  ID3s <- other_ID2s$ID_3 #creates a character list of the ID_3's associated with that ID_2
  ID3_nums <- gsub(paste0(ID_2, "."), "", ID3s) #Strips the ID_3's to the sequential numbers
  ID3_nums <- as.numeric(ID3_nums) #Converts the numbers to numeric
  ID3_nums <- c(ID3_nums, 0) #Adds a 0 to the list - has no effect if there are other numbers present, but if this is the first time this ID_2 is being used then it creates a number to use for the next step
  next_ID3 <- max(ID3_nums) + 1 #Finds the maximum sequential ID_3 number used so far, and adds 1 to it
  if(nchar(next_ID3) == 1){ #This checks if its a single digit number or not - if it is, the next_ID3 sequential number is converted to a character and a 0 is added preceeding it
    next_ID3 <- paste0("0", next_ID3)
  }
  new_ID_3 <- paste0(ID_2, ".", next_ID3) #the new ID_3 is created from the calculated next ID_3 sequential number, and the ID_2 input
  return(new_ID_3)
}


New_Dict_Entry <- function(dictionary, ID_2, FoodName_3, scientific_name, FoodEx2_code, Description1 = "", Desc1.ref = "", Description2 = "", Desc1.ref2 = ""){
  
  new_ID_3 <- Find_ID_3(dictionary = dictionary, ID_2 = ID_2) #Finds the next ID_3
  
  #Next stage finds and copies ID_0 and ID_1 based on existing ID_2's
  other_ID2s <- dictionary[dictionary$ID_2 == ID_2,] #narrows down the dictionary to all entries which have the same ID_2
  other_ID2s <- other_ID2s[!is.na(other_ID2s$ID_2),] #Removes NA's
  if(nrow(other_ID2s) == 0){
    stop('This is a unique ID_2, and therefore ID_0 and ID_1 cannot be copied. Please enter manually.')
  }
  copied_row <- other_ID2s[1,]
  copied_row$ID_3 <- new_ID_3
  copied_row$FE2_3 <- FoodEx2_code
  copied_row$FoodName_3 <- FoodName_3
  copied_row$Description1 <- Description1
  copied_row$Desc1.ref <- Desc1.ref
  copied_row$Description2 <- Description2
  copied_row$scientific_name <- scientific_name
  copied_row$Desc1.ref2 <- Desc1.ref2
  
  new_dictionary <- rbind(dictionary, copied_row)
  new_dictionary <- new_dictionary[order(new_dictionary$ID_1),]

  return(new_dictionary)
}
