
library(tidyverse)

###-------------------------LOADING FCT DATASET----------------------#####

FCT <-  read.csv(here::here('data', 'FCT_10.csv'))

mwi_mn_raw <- readxl::read_excel(here::here('data',
                                            '2015_Joy_crop-min-composition.xlsx'), 
                                 sheet = 'STable7', skip = 2)

######---------------------1) SOP calculation -------------------------#########


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



#Calculating SOP for the WAFCT


WAFCT <- WAFCT %>%
  mutate(SOP_cal = case_when(
    !is.na(FAT) & !is.na(FIBTG) ~  reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`),
    is.na(FAT) & !is.na(FIBTG) ~ reduce(select(.,
                                               'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`),
    !is.na(FAT) & is.na(FIBTG) ~ reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`),
    TRUE  ~ reduce(select(., 'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`)))




WAFCT <- WAFCT %>%
  mutate(SOP_cal = case_when(
    (!is.na(FAT) & !is.na(FIBTG)) ~  reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`),
    (is.na(FAT) & !is.na(FIBTG)) ~ reduce(select(., 'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`),
    (!is.na(FAT) & is.na(FIBTG)) ~ reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`),
    TRUE  ~ reduce(select(., 'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`)))

WAFCT <- WAFCT %>%
  mutate(SOP_cal = ifelse((!is.na(FATCE) & !is.na(FIBC)),  reduce(select(., 'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`),
                          ifelse((is.na(FAT) & !is.na(FIBTG)), reduce(select(., 'WATER', 'PROTCNT' ,'FATCE', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`),
                                 ifelse((!is.na(FAT) & is.na(FIBTG)) ~ reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBC', 'ALC', 'ASH'), `+`),
                                        reduce(select(., 'WATER', 'PROTCNT' ,'FAT', 'CHOAVLDF','FIBTG', 'ALC', 'ASH'), `+`)))))



#######----------------2) ASH calculation -----------------------##############

#2.1) Check availability and unit of all the minerals needed

FCT <- FCT %>% rowwise %>% mutate( CL_cal = `NA.`* 2.5, 
                                   MN_mg = MN/1000 ) #Only for MAFOODS

#2.2) Calculate the sum of all minerals

#The formula below is not valid, we need to change so it calculate
#when na.rm = TRUE

FCT <- FCT %>%
  mutate(ASH_cal = reduce(select(.,
                                 'CA', 'FE', 'CL_cal', 'MN_mg',
                                 'MG',  'P', 'K', 'NA.', 'ZN',
                                 'CU'), `+`))

#Check when ASH_cal is > than ASH = TRUE then is.low.quality = TRUE
#ASH_cal < than ASH can be due to lack of min data




MAFOODS <-read.csv(here::here('data', 'MAPS_MAFOODS_v01.csv'))

MAFOODS %>%
  ggplot(aes(CA, foodgroup)) + geom_boxplot()

#Create a function with a loop to see all the variables of interest (mineral mn)
#sorted by foodgroup
#we can use this f(x) for other dataset

plotBoxFunc <- function(x, na.rm = TRUE, ...) {
  nm <- c("CA", "CU", "FE", "MG", "SE", "ZN")
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + 
      geom_boxplot(aes(y =  foodgroup))
    ggsave(plots,filename=paste("plot",nm[i],".png",sep=""))
  }
}


plotBoxFunc(MAFOODS) ## execute function






MAFOODS %>% dplyr::filter(CA >700) %>%  pull(fooditem, ref)

######-------------------4) VARIABILITY calculation -------------#####

#Re-name variables

mwi_mn <- mwi_mn_raw %>% rename(fooditem = '...1', 
                                foodtissue = '...2', 
                                foodnotes = '...3', 
                                soiltype = '...4', 
                                n_sample = 'Ca',
                                ca_mean = '...7', 
                                ca_sd = '...8',
                                ca_median = '...9',
                                cu_mean = '...16', 
                                cu_sd = '...17',
                                cu_median = '...18',
                                fe_mean = '...25',
                                fe_sd = '...26',
                                fe_median = '...27', 
                                mg_mean = '...34',
                                mg_sd = '...35',
                                mg_median = '...36',
                                se_mean = '...43',
                                se_sd = '...44',
                                se_median = '...45',
                                zn_mean = '...52',
                                zn_sd = '...53',
                                zn_median = '...54')

#Filtering items with combined data and more than 1 sample

mwi_mn <-  mwi_mn %>% 
  filter(soiltype == 'Combined', n_sample != "1") %>% 
  select(!starts_with('...'))

mn <- c('ca', 'cu', 'fe', 'mg', 'se', 'zn')

mwi_mn <-  mwi_mn %>% mutate_at(vars(starts_with(mn)), as.numeric)

#Trying to identify food items with high variability
#we want to flag possible quality issues w/i the data

#Checking 3-sd > mean

mwi_mn <-  mwi_mn %>% mutate(is_low_quality = case_when(
  ca_mean < (ca_sd*3) ~ 'ca',
  cu_mean < (cu_sd*3) ~ 'cu', 
  fe_mean < (fe_sd*3) ~ 'fe', 
  mg_mean < (mg_sd*3) ~ 'mg',
  se_mean < (se_sd*3) ~ 'se', 
  zn_mean < (zn_sd*3) ~ 'zn',
  TRUE ~ 'NO'
))

#Checking 2-sd > mean


mwi_mn <-  mwi_mn %>% mutate(is_low_quality = case_when(
  ca_mean < (ca_sd*2) ~ 'ca',
  cu_mean < (cu_sd*2) ~ 'cu', 
  fe_mean < (fe_sd*2) ~ 'fe', 
  mg_mean < (mg_sd*2) ~ 'mg',
  se_mean < (se_sd*2) ~ 'se', 
  zn_mean < (zn_sd*2) ~ 'zn',
  TRUE ~ 'NO'
))

#Checking sd > mean


mwi_mn <-  mwi_mn %>% mutate(is_low_quality = case_when(
  ca_mean < ca_sd ~ 'ca',
  cu_mean < cu_sd ~ 'cu', 
  fe_mean < fe_sd ~ 'fe', 
  mg_mean < mg_sd ~ 'mg',
  se_mean < se_sd ~ 'se', 
  zn_mean < zn_sd ~ 'zn',
  TRUE ~ 'NO'
))

mwi_mn %>% filter(is_low_quality == 'NO')


######---------------------END -------------#####

