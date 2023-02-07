
#Loading data and library needed

#Loading the standardised FCT
output_table <- read.csv(here::here("inter-output", "UK21_FCT_FAO_Tags.csv"))
#Loading the food dictionary
source(here::here("MAPS_Dictionary-Protocol.R"))
#Loading functions
source(here::here("functions.R"))
#Loading formatting data
var.name <- read.csv(here::here("metadata", "fct-variable-names.csv")) %>% 
  select(Column.Name) %>% pull()


names(output_table)
names(dictionary.df)

#Fuzzy_Matcher(output_table[, c(1:2)], dictionary.df[, c(7, 9)])
#names(fuzzy_uk_dictionary)[1:2] <- names(output_table)[1:2]
#names(fuzzy_uk_dictionary)[3:4] <- names(dictionary.df)[c(7, 9)]
#names(fuzzy_uk_dictionary)
#output_table %>% left_join(.,fuzzy_uk_dictionary)
#write.csv(fuzzy_uk_dictionary, here::here("inter-output", "ukfct_matches.csv"))


genus <- tribble(
  ~ref_fctcode,   ~ID_3, ~confidence,
  "11-886", "23110.02", "m", 
  "11-002" ,  "23140.05.01", "h", 
  "11-788",  "23140.07.01", "l", 
  "13-086", "1701.02", "l", 
  "14-340", "F0262.01", "l",
  "17-034", "2168.01", "h", 
  "17-039",  "2165.01", "h", 
  "17-031", "2166.01", "h", 
  "17-043", "21691.07.01", "m",
  "17-686", "34550.01", "m", 
  "14-384", "1321.02", "m", 
  "14-319", "1341.01", "m", 
  "17-165", "23914.01", "m", # Black tea average
  "17-749", "24310.01.01", "m", 
  "18-488", "21121.01", "h", 
  "18-600",  "21521.01", "l", 
  "16-007",  "1501.02", "m", 
  "16-439", "1532.01", "l",
  "13-340", "1594.01", "l", # nori, dried TBC
  "16-492", "1518.01", "m", 
  "13-582", "1212.01", "h",
  "17-170", "1620.01", "l",  # tea leaves infusion
  "17-171", "23914.04", "h", 
  "17-172", "23914.05", "h",
  "14-125", "1329.01", "h", 
  "17-515", "F1232.11", "h" ,
  "17-726", "F1232.12", "h",
  "17-727", "F1232.13", "h", 
  "17-672", "24490.03", "h", 
  "12-390", "22270.06", "m", 
  "12-389", "21439.9.04", "m",
  "17-767", "24310.01.03", "h", 
  "11-824", "23999.02.01", "m", 
  "17-645", "23999.02.02", "m", 
  "17-642", "23999.02.03", "m",
  "13-293", "1270.02", "h", 
  "16-279", "1520.07", "m", 
  "13-607", "21329.01", "h",
  "13-382", "21321.01", "h",
  "19-649", "21181.01", "m",
 # "13-881", 
)

#Combining codes from fuzzy matcher and manually added

genus <- read.csv(here::here("inter-output", "ukfct_matches.csv")) %>% 
  select(fdc_id, ID_3, Confidence) %>% 
  mutate(confidence = case_when(
    Confidence == "high" ~ "h", 
    Confidence == "medium" ~ "m", 
    Confidence == "low" ~ "l", 
    TRUE ~ Confidence)) %>% select(-Confidence) %>%
  # mutate_at("FCT.code", as.character) %>% 
  rename(ref_fctcode = "fdc_id") %>% 
  bind_rows(genus) %>% distinct()

#Checking for duplicates

(dupli <- genus %>%  count(ref_fctcode) %>% 
    filter(n>1) %>% pull(ref_fctcode))

#Updating the dictionary compilation -----
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v"),
             decreasing = T)[2]
genus %>% mutate(fct = "UK21")  %>% 
  bind_rows(., read.csv(here::here("metadata", file)) %>%
              mutate_at("ref_fctcode", as.character)) %>% distinct() %>% 
  write.csv(., here::here("metadata", file), row.names = F)

#Adding food dictionary codes to FCT ----

output_table <- output_table %>% 
  left_join(., genus, by = c("fdc_id" = "ref_fctcode")) %>% 
  relocate(ID_3, .after = food_desc)

dim(output_table)

## CHECK: Adding new food dictionary code ----

#Checking dictionary/ fct ids availability 
subset(output_table, fdc_id == "13-881", select = c(food_desc, ID_3)) 
subset(output_table, fdc_id %in% c("11-824", "17-645",
                                   "17-642"), select = c(food_desc, ID_3)) 
subset(output_table, ID_3 == "2351F.01") 

dictionary.df %>% filter(ID_3 == "23914.05")
subset(dictionary.df, ID_2 == "1651")
subset(dictionary.df, ID_1 == "2782")
subset(dictionary.df, ID_0 == "PB")

subset(output_table, 
       grepl("juice", food_desc, ignore.case = TRUE)&
      grepl("^13", fdc_id), 
        select = c(fdc_id, food_desc, ID_3, WATERg))

subset(output_table, grepl("bacon", food_desc, ignore.case = TRUE) &
       grepl("raw", food_desc, ignore.case = TRUE), 
       select = c(fdc_id, food_desc, ID_3, WATERg))

subset(dictionary.df, 
       grepl("bacon", FoodName_2, ignore.case = TRUE) &
         grepl("",  FoodName_3, ignore.case = TRUE))

#Rename variables according to MAPS-standards

MAPS_output <- output_table %>% select(-food_group) %>% #removing original food group variable from UKFCT
  left_join(.,dictionary.df %>% 
              select(ID_3, FoodName_3, 
                     FoodName_0, FoodName_1) %>%
              filter(str_detect(ID_3, "\\b"))) %>% 
  mutate_at(c("RETOLmcg", "CARTBEQmcg"), as.numeric) %>% 
  VITA_RAEmcg_std_creator() %>% 
  rename(
    original_food_id = "fdc_id",
    original_food_name = "food_desc",
    food_genus_id = "ID_3",
    food_genus_description = "FoodName_3",
    food_group = "FoodName_0",
    food_subgroup = "FoodName_1", 
    food_genus_confidence = "confidence",
    fct_name = "source_fct",
    data_reference_original_id = "nutrient_data_source",
    moisture_in_g = "WATERg",
    energy_in_kcal = "ENERCkcal",
    energy_in_kj = "ENERCkJ",
    totalprotein_in_g = "PROCNTg",
    nitrogen_in_g = "NTg", 
    totalfats_in_g = "FAT_g",
    saturatedfa_in_g = "FASATg", 
    monounsaturatedfa_in_g = "FAMSg", 
    polyunsaturatedfa_in_g = "FAPUg", 
    cholesterol_in_mg = "CHOLmg",
    carbohydrates_in_g = "CHOAVLg", 
    fibre_in_g = "FIBTGg", 
    ca_in_mg = "CAmg", 
    fe_in_mg = "FEmg",
    mg_in_mg = "MGmg",
    p_in_mg = "Pmg",
    k_in_mg = "Kmg",
    na_in_mg = "NAmg", 
    zn_in_mg = "ZNmg",
    se_in_mcg = "SEmcg",
    i_in_mcg = "IDmcg",
    cu_in_mg = "CUmg",
    mn_in_mcg = "MNmg",
    vitamina_in_rae_in_mcg = "VITA_RAEmcg_std", #Re-calculated
    thiamin_in_mg = "THIAmg",
    riboflavin_in_mg = "RIBFmg", 
    niacin_in_mg = "NIAmg", 
    folate_in_mcg = "FOLmcg", # This should be reviewed
    vitaminb12_in_mcg = "VITB12mcg",
    vitaminc_in_mg = "VITCmg",
    phytate_in_mg = "Total.PHYTO", 
    vitaminb6_in_mg = "VITB6_mg",
    pantothenate_in_mg = "PANTACmg",
    biotin_in_mcg = "BIOTmcg",
    vitamind_in_mcg = "VITDmcg",
    vitamine_in_mg = "VITEmg") %>% 
  mutate(
    folicacid_in_mcg = "NA", 
    ash_in_g = "NA") %>% select(var.name)


#Checking for duplicated items
dim(MAPS_output)
which(duplicated(MAPS_output))

#Checking duplicates in dictionary codes
sum(duplicated(MAPS_output$food_genus_id[!is.na(MAPS_output$food_genus_id)]))

#Checking that all dictionary codes have been matched to an entry in the dictionary
subset(MAPS_output, !is.na(food_genus_id) & is.na(food_genus_description))

#Saving file into csv to be used in MAPS tool
readr::write_excel_csv(MAPS_output, here::here('output', 
                                               'MAPS_UKFCT_v1.1.csv'))











