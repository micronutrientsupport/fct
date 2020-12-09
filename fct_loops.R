

library(tidyverse)

##Trying out loops


for (mineral in c('CA','FE', 'MG', 'P', 'K', 'NA', 'ZN', 'CU')) {
  new_col_name <- paste0("low_quality_", mineral)
  minerals_WAFCT <- WAFCT %>% select(CA , FE, MG, P, K, `NA`, ZN, CU) %>% 
    mutate(!!sym(new_col_name) := map(., ~str_detect(., '\\[.*?\\]')))
}

for (mineral in c('CA','FE', 'MG', 'P', 'K', 'NA', 'ZN', 'CU')) {
  new_col_name <- paste0("low_quality_", mineral)
  WAFCT <- WAFCT %>% 
    mutate(!!sym(new_col_name) := str_detect(mineral, '\\[.*?\\]'))
}



for (mineral in c('CA','FE', 'MG', 'P', 'K', 'NA', 'ZN', 'CU')) {
  WAFCT <- WAFCT %>%
    mutate(!!sym(mineral) := str_remove(mineral, '\\['))
}

library(tidyxl)

x <- tidyxl::xlsx_cells(here::here('data', '2019_MAFOODS.xlsx'), 'Malawian Master FCDB')

formats <- tidyxl::xlsx_formats(here::here('data', '2019_MAFOODS.xlsx'), 'Malawian Master FCDB')

#Provides TRUE/FALSE if there is bold formating in the table format styles

formats$local$font$bold

formats$local$font$color$rgb

#Provides the cell (in excel = address) and the numeric value (if numeric) of those values
#formatted as italic in MAFOODS data set. 

x[x$local_format_id %in% which(formats$local$font$italic), c("address", "numeric")]

#Provides the cell (in excel = address) and the numeric value (if numeric) of those values
#formatted as in blue colour (FF2F5496 or FF4472C4)  in MAFOODS data set.

x[x$local_format_id %in% which(formats$local$font$color$rgb == 'FF2F5496'), c("address", "numeric")]

x[x$local_format_id %in% which(formats$local$font$color$rgb == 'FF4472C4'), c("address", "numeric")]


###==============LOOPING GRAPHS =================#########

#Adapted from Kevin
#Not working for my case

mn <- c("CA", "CU", "FE", "MG", "SE", "ZN")

for (i in mn) {
  temp_plot = ggplot(MAFOODS, aes(i, foodgroup)) + 
    geom_boxplot() +
    labs(y = "minerals") 
  
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#Loop from r-bloggers (ggplot2 graphics in a loop)
#Working but it is for histogram

plotHistFunc <- function(x, na.rm = TRUE, ...) {
  nm <- c("CA", "CU", "FE", "MG", "SE", "ZN")
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "dodgerblue")
    ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""))
  }
}

plotHistFunc(MAFOODS) ## execute function

#Addapted version of the previous one for boxplot 
#And y = sorting variable

plotBoxFunc <- function(x, na.rm = TRUE, ...) {
  nm <- c("CA", "CU", "FE", "MG", "SE", "ZN")
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + 
      geom_boxplot(aes(y =  foodgroup))
    ggsave(plots,filename=paste("plot",nm[i],".png",sep=""))
  }
}


plotBistFunc(MAFOODS) ## execute function


#Working on a f(x) to remove all []


b <- WAFCT %>% mutate(FE = case_when(
  str_detect(FE, '\\[.*?\\]') == TRUE ~ str_extract(FE, '(?<=\\[).*?(?=\\])'), 
  TRUE ~ FE))


no_brackets <- function(i){
  case_when(
  str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'), 
  TRUE ~ i)
  }

fat <- WAFCT$FAT

no_brackets(fat)

no_brackets(WAFCT$FAT)



