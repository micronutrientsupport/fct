

library(tidyverse)


##--- Loading data

FCT <- read_csv(here::here('data', 'FCT_10.xlsx'))
                   

#Checking distribution of micronutrient quantity in different food items:

FCT %>% filter(str_detect(fooditem, 'rice')) %>%
  ggplot(aes(FCT, ZN)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem, 'wheat')) %>%
  ggplot(aes(FCT, ZN)) + geom_boxplot()

FCT %>% filter(FCT != 'NGAFCT') %>%
  filter(str_detect(fooditem, 'Maize|Corn|corn|maize')) %>%
  ggplot(aes(FCT, ZN)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem, 'corn|maize')) %>%
  ggplot(aes(FCT, FE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem, 'corn|maize')) %>%
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()


FCT %>% filter(str_detect(fooditem, 'corn|maize')) %>%
  ggplot(aes(FCT, WATER)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem, 'wheat')) %>%
  ggplot(aes(FCT, WATER)) + geom_boxplot()


FCT %>% filter(str_detect(fooditem, 'fortif')) %>%
group_by(FCT, foodgroup) %>% summarise(n = length(fooditem))


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% pull(fooditem)

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
 filter(str_detect(fooditem ,' fortified')) %>% pull(fooditem)

FCT %>% filter(str_detect(fooditem, 'Maize|Corn|corn|maize')) %>% filter(FE>5.4) %>%
  ggplot(aes(FCT, FE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem, 'Maize|Corn|corn|maize')) %>% filter(FE>5.4) %>% pull (fooditem)

FCT %>% filter(str_detect(fooditem, 'Maize|Corn|corn|maize')) %>% filter(ZN > 2.7) %>% pull (fooditem, FCT)

FCT %>% filter(str_detect(fooditem, 'corn|maize')) %>% filter(FE<5.4) %>% pull (fooditem)

FCT %>% filter(str_detect(fooditem, ' fortified')) %>% pull(fooditem)

FCT %>% filter(str_detect(fooditem, '(Chimanga)')) %>% pull(SE)


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'fortified with vitamin A')) %>%
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'fortified with vitamin A')) %>%
  summarise(ave = mean(VITA), min_vita = min(VITA))

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(VITA > 0) %>%
  summarise(ave = mean(VITA_RAE), min_vita = min(VITA_RAE))


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  pull(fooditem)

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(VITA_RAE <171) %>%
  summarise(ave = mean(VITA_RAE, na.rm = TRUE), max_vita = max(VITA_RAE, na.rm = TRUE))


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(!str_detect(fooditem ,' fortified')) %>% 
  filter(VITA_RAE <171) %>%
  summarise(ave = mean(VITA_RAE, na.rm = TRUE), max_vita = max(VITA_RAE, na.rm = TRUE))

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(!str_detect(fooditem ,' fortified')) %>% 
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(!str_detect(fooditem ,' fortified')) %>% 
  filter(VITA_RAE <10) %>%
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>%
  ggplot(aes(VITA_RAE)) + geom_histogram()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  ggplot(aes(VITA_RAE)) + geom_histogram()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(!str_detect(fooditem ,'fortified with vitamin A')) %>% 
  filter(!str_detect(fooditem ,' fortified')) %>% 
  ggplot(aes(VITA_RAE)) + geom_histogram()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'unfortified')) %>% 
  ggplot(aes(VITA_RAE)) + geom_histogram()


FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'unfortified')) %>% 
  filter(VITA_RAE <171) %>%
  summarise(ave = mean(VITA_RAE, na.rm = TRUE), max_vita = max(VITA_RAE, na.rm = TRUE))

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'unfortified')) %>% 
  ggplot(aes(FCT, VITA_RAE)) + geom_boxplot()

FCT %>% filter(str_detect(fooditem ,'Maize|Corn|corn|maize')) %>% 
  filter(!str_detect(fooditem ,'oil')) %>% 
  filter(str_detect(fooditem ,'unfortified')) %>% 
  filter(VITA_RAE > 10) %>%
  pull(fooditem)