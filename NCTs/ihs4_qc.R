

# Load libraries
library(dplyr)

# Loading data
dat <- read.csv(here::here("inter-output", "hces", "ihs4.cons.csv"))[,2:4]
food_list <- readRDS(here::here("inter-output", 
                      sort(list.files(here::here("inter-output"), 
                            "MAPS_food-list_ihs4"), decreasing = TRUE)[1])) 



# Checking the data
names(dat)
head(dat)
dim(dat)
length(unique(dat$code))

#dat$item[which(is.na(dat$VitaminA_in_RAE_in_mcg))]

dat$g_consumed <- as.numeric(dat$g_consumed)

dat %>% filter(item_code == "502" & g_consumed >0)

#Missing items:
# No consumption reported (116, 314, 708)
# Popcorn (835)

## Adjusting quantity of apparent consumption to FCT matches

food <- dat %>% 
  left_join(., food_list[,c("code", "item", "ID_3", "food_genus_confidence", "wt")], 
                  by =c("item_code" = "code"))

  food %>% filter(g_consumed>0 & (is.na(ID_3) |is.na(food_genus_confidence))) %>% 
    distinct(item_code)
  
#New variable for the changed consumption
food$amount_consumed_std_in_g <- food$g_consumed*food$wt

##Checking if there are wrong values, i.e., values with multiple matches
# With

for(i in 1:nrow(food)){
  
  if(!is.na(food$amount_consumed_std_in_g[i])){
if(food$wt[i]<1 & food$g_consumed[i]>0){

  if(food$amount_consumed_std_in_g[i] == food$g_consumed[i])
  stop(paste("Weight not applied in line", i))

}else{
  
  if(food$wt[i]==1){
    if(food$amount_consumed_std_in_g[i] > food$g_consumed[i])
      stop(paste("Standardised consumption higher in row", i))
  }
}
}else{
    next
  }    
next
  }
  
  # Peanuts fresh to peanuts dried --> Important due to the water content. 
  # Adjust the quantity to dry E.g., 100g of fresh weight will be 100*(100-50)/(100-10)
food$amount_consumed_std_in_g[food$ID_3 == "142.03" & !is.na(food$amount_consumed_std_in_g)] <- food$amount_consumed_std_in_g[food$ID_3 == "142.03"& !is.na(food$amount_consumed_std_in_g)]*(100-50)/(100-10)
food$ID_3[food$ID_3 == "142.03"] <- "142.01"

# Catfish from fresh to dried. --> Important due to the water content. 
# Adjust the quantity to fresh E.g., 100g of dry weight will be qty*(100-DW)/(100-FW)
#Water values based on catfish WA19(09_060) - 78g (FW) and MW19(MW03_0048) - 21g (DW)
food$amount_consumed_std_in_g[food$ID_3 == "1505.01" & !is.na(food$amount_consumed_std_in_g)] <- food$amount_consumed_std_in_g[food$ID_3 == "1505.01" & !is.na(food$amount_consumed_std_in_g)]*(100-21)/(100-78)
food$ID_3[food$ID_3 == "1505.01"] <- "1503.07"

