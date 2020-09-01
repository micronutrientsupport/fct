
library(tidyverse)

###-------------------------LOADING FCT DATASET----------------------#####

FCT <-  read.csv(here::here('data', 'FCT_10.csv'))

######----------------------------------------------------------#########


#Data set with SOP mean, max and min per FCT and

SOP_sum <- FCT %>% group_by(FCT) %>%  summarise(no = length(fooditem),
                                                mean_SOP = mean(SOP, na.rm = TRUE),
                                                sd_SOP = sd(SOP, na.rm = TRUE),
                                                min_SOP = min(SOP, na.rm = TRUE),
                                                max_SOP = max(SOP, na.rm = TRUE))

#Preferable level (1) for SOP 97-103g

FCT <-  FCT %>% mutate(low_quality_SOP = case_when(
  is.na(SOP) ~ NA,
  SOP  <= 97 ~ TRUE,
  SOP >= 103 ~ TRUE,
  TRUE ~  FALSE))


FCT %>% group_by(FCT) %>% filter(low_quality_SOP == TRUE) %>%
  summarise(no_low_quality_SOP1 = length(fooditem), 
              mean_low_quality_SOP1 = mean(SOP))


FCT %>% group_by(FCT, low_quality_SOP) %>%
  summarise(no_low_quality_SOP1 = length(fooditem), 
            mean_low_quality_SOP1 = mean(SOP))

#Acceptable level (2) for SOP 95-105g

FCT <-  FCT %>% mutate(low_quality_SOP = case_when(
  is.na(SOP) ~ NA,
  SOP  <= 95 ~ TRUE,
  SOP >= 105 ~ TRUE,
  TRUE ~  FALSE))

FCT %>% group_by(FCT, low_quality_SOP) %>%
  summarise(no_low_quality_SOP2 = length(fooditem), 
            mean_low_quality_SOP2 = mean(SOP))




#Data set with all the missing values per FCT and variable

missing <- FCT %>% filter(!is.na(fooditem)) %>% group_by (FCT) %>%
  summarise_all(funs(sum(is.na(.))))

missing_MN <- FCT %>% filter(!is.na(fooditem)) %>%
  group_by (FCT) %>% 
  summarise_at(vars('VITA_RAE', 'VITA', 'CARTB', 'VITC', 
                    'VITB12', 'FOL', 'FOLDFE','FIBTG' , 'FIBC',
                    'FIBTS' ,'SE',  'ZN', 'ID', 'FE' , 'CA', 'PHYT',
                    'PHYTCPP', 'PHYTCPPD_I', 'PHYTAC', 'SOP'), funs(sum(is.na(.))))


#Data set with SOP mean, max and min per FCT and

SOP_sum <- FCT %>% group_by(FCT) %>%  summarise(no = length(fooditem),
                                                mean_SOP = mean(SOP, na.rm = TRUE),
                                                sd_SOP = sd(SOP, na.rm = TRUE),
                                                min_SOP = min(SOP, na.rm = TRUE),
                                                max_SOP = max(SOP, na.rm = TRUE))

write_csv(missing_MN, here::here('data' ,'missing_MN.csv'))

write_csv(SOP_sum, here::here('data' ,'SOP.csv'))


#Quality checks - Variability of SOP

FCT %>% ggplot(aes(FCT, SOP)) + geom_boxplot() 

FCT %>% filter(FCT != 'ETHFCT') %>% 
  ggplot(aes(FCT, SOP)) + geom_boxplot() 

#Variability of key MN (minerals) by FCT

FCT %>% ggplot(aes(FCT, ZN)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, SE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, FE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, ID)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, CA)) + geom_boxplot() 

#Variability of key MN (vitamins) by FCT

FCT %>% ggplot(aes(FCT, VITA_RAE)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, VITB12)) + geom_boxplot() 

FCT %>% ggplot(aes(FCT, VITC)) + geom_boxplot() 


# % of missing values

naniar::vis_miss(FCT)

#heatmap of missing values per FCT

naniar::gg_miss_fct(FCT, fct = FCT)

#heatmap of missing values of the key MNs by FCT
#And saving it as png

png("heatmap.png", width = 6, height = 4, units = 'in', res = 300)

FCT %>% select('FCT', 'VITA_RAE', 'VITA', 'CARTB', 'VITC', 'VITB12', 'FOL', 'FOLDFE','FIBTG' , 'FIBC',
               'FIBTS' ,'SE',  'ZN', 'ID', 'FE' , 'CA', 'PHYT', 'PHYTCPP', 'PHYTCPPD_I', 'PHYTAC', 'SOP') %>% 
  naniar::gg_miss_fct(fct = FCT)

dev.off()


FCT %>% select('FCT', 'SE',  'ZN', 'ID', 'FE' , 'CA') %>% 
  naniar::gg_miss_fct(fct = FCT)


FCT06 <- FCT %>% filter(!FCT %in% c('ETHFCT','NGAFCT', 'GMBFCT', 'UGAFCT') )

write.csv(FCT06,  here::here('data', 'FCT_06.csv'))