
#install.packages("xlsx")

library(tidyverse)


###============================ DO NOT RUN =========================####

fct_metadata<- read.csv(here::here( "fct_metadata.csv"))

fct_metadata$Variable_Name <-  str_replace(fct_metadata$Variable_Name, "in_.g$|in_kcal|in_kj", "method")

fct_metadata %>% mutate(Variable_Name = str_replace(Variable_Name, "_in_.*g$|_in_kcal|_in_kj|_cal",
                                                    "_method"), 
                        Variable_Name = str_replace(Variable_Name, 
                            "vitamina_method", "vitamina_rae_method")) %>% 
  write.csv(here::here("fct_metadata_v1.0.csv"), row.names = FALSE)




###============================  END  ===============================####

geonetwork <- readxl::read_excel(here::here('metadata', 'Geonetwork-Metadata.xlsx'), sheet = 1)

cc <- raster::ccodes() 

fct_metadata_str <- read.csv(here::here( "fct_metadata_v1.0.csv"))

FCT_QA <- readxl::read_excel(here::here('data', 'FCT_QA.xlsx'), sheet = 2)

metadata_variables <- fct_metadata_str  %>%  pull(Variable_Name)

var.metadat <- fct_metadata_str %>% spread(Variable_Name, 
                                           Variables_Description) %>% 
  mutate_all(as.numeric) %>%  mutate_all(as.character)

fct_metadata <- FCT_QA %>% select(Name:Recipes,
                                  -c(Contact,
                                     Contact_email, 
                                     FoodCategories,
                                     CategoryList, Key_MN,
                                     CARTB_cal )) %>% 
  rename(
    fct_name = "Name", 
    fct_short_name = "Short_name",
    fct_authors = "Authors",      
    fct_region = "Country/Region",
    fct_lead_organization = "LeadOrganization",
    fct_year = "Year",       
    fct_language = "Language" , 
    fct_data_format = "DataFormat",
    fct_documentation = "Metadata",       
    fct_documentation_link = "Link",
    fct_licence = "Licence",
    fct_data_sources = "DataSources",   
    fct_fooditem = "FoodItems",
    fct_component = "Component" ,
    fct_component_list = "ComponentList",
    edible = "EDIBLE", 
    energy_kcal_method = "ENERC_cal", 
    totalprotein_method = "﻿PROT_cal",        
    carbohydrates_method = "CHO_cal",
    fibre_method = "FIBT_cal" ,
    folate_method = "FOLDFE_cal",    
    folicacid_method = "FOL_cal",
    vitaminb12_method = "VITB12_cal",
    vitamina_rae_method = "VITA_cal",    
    vitaminc_method = "VITC_cal",
    i_method = "ID_cal",         
    zn_method = "ZN_cal",
    se_method = "SE_cal",
    fe_method = "FE_cal",
    ca_method = "CA_cal",
    phyticacid_method = "PHYTAC_cal",
    edible_source = "EDIBLE_ref",    
    yieldfactor = "YF",
    yieldfactor_source = "YF_ref",
    retentionfactor = "RF",
    retentionfactor_source = "RF_ref",
    recipe_method = "Recipes" ) %>%
   mutate_at(c("fct_year", "fct_component"), as.character) %>% 
  left_join(., var.metadat) 

#%>% 
#  write.csv(here::here("fct_metadata_v1.1.csv"), row.names = FALSE)


#Adding metadata for regional-fct

fct_metadata <- fct_metadata %>% add_row(
  fct_name = "Supplementary Table 2. Food mineral composition data from literature sources, used in conjunction with Food Balance Sheets (FBSs) to estimate dietary mineral availability", 
  fct_short_name = "Eastern-Africa, Middle-Africa, Southen-Africa, Western-Africa",
  fct_authors = "Edward J. M. Joy, E. Louise Ander, Scott D. Young, Colin R. Black, Michael J. Watts, Allan D. C. Chilimba, Benson Chilima, Edwin W. P. Siyame, Alexander A. Kalimbira, Rachel Hurst, Susan J. Fairweather-Tait, Alexander J. Stein, Rosalind S. Gibson, Philip J. White, Martin R. Broadley",      
  fct_region = "sub-Saharan Africa",
  fct_lead_organization = "British Geological Survey, University of Nottingham",
  fct_year = "2014",       
  fct_language = "EN" , 
  fct_data_format = "xlsx",
  fct_documentation= "Joy et al, 2014. Physiologia Plantarum, Volume 151, Issue3, Pages 208-229",       
  fct_documentation_link = "https://doi.org/10.1111/ppl.12144",
  fct_licence = "This is an open access article under the terms of the Creative Commons Attribution License, which permits use, distribution and reproduction in any medium, provided the original work is properly cited.",
  fct_data_sources = "scientific literature, FCT") 
#%>% 
#  write.csv(here::here("fct_metadata_v1.2.csv"), row.names = FALSE)

#Changing variable fct_documentation to fct_documentation_citation
#create multple records for fct-regional and removing fct not used in the tool.

fct_metadata <- fct_metadata %>% 
  rename(fct_documentation_citation = "fct_documentation") %>% 
  filter(fct_short_name %in% c("MAFOODS", "WAFCT", "LSOFCT", "KENFCT", 
              "Eastern-Africa, Middle-Africa, Southen-Africa, Western-Africa")) %>% 
  separate_rows(fct_short_name, sep = ",") %>%
  mutate_all(., str_squish) 

africa.region <- cc %>% filter(continent == "Africa") %>% 
  group_by(UNREGION1) %>% summarise(ISO = paste(ISO3, collapse = ","))

fct_metadata <- fct_metadata %>% 
  mutate(fct_region = case_when(
    fct_short_name == "Eastern-Africa" ~ africa.region$ISO[1],
    fct_short_name == "Middle-Africa" ~ africa.region$ISO[2],
    fct_short_name == "Southen-Africa" ~ africa.region$ISO[4],
    fct_short_name == "Southen-Africa" ~ africa.region$ISO[5], 
    TRUE ~ fct_region))
  
#fct_metadata %>% mutate_all(funs(replace_na(., "NULL"))) %>% 
#   mutate_all(funs(str_replace_all(.,"NA", "NULL")))

fct_metadata %>% mutate_all(funs(replace_na(., ""))) %>% #saving for MAPS
  mutate_all(funs(str_replace_all(.,"NA", ""))) %>%      #changing NA for whitespaces
  write.csv(here::here("output", "fct_metadata_v1.5.csv"), row.names = FALSE)

x <- meta %>% select(fct_name, fct_region, fct_lead_organization, fct_year,
                fct_documentation_link, fct_licence) %>% 
  mutate_at("fct_year", as.character) %>% 
  mutate(start_date = fct_year, 
         end_date = "current",
         edition = fct_year, 
         abstract = c("food composition for use in West-Africa", 
                      "food compositon for use in Malawi",
                      "food compositon for use in Kenya", 
                      "food compositon for use in Lesotho", 
                      "mineral food composition for use in Eastern-Africa",
                      "mineral food composition for use in Middle-Africa",
                      "mineral food composition for use in Southen-Africa",
                      "mineral food composition for use in Southen-Africa"
                      )) %>% 
      mutate(status = "completed", 
         contact_type = "Originator", 
          maintainance = "Unknown", 
         keywords = "food composition table, FCT, food composition databases,
                       nutritonal composition of food, Africa", 
         constraint_text = "NA", 
        metadata_contact_name = "Lucia Segovia de la Revilla", 
        metadata_contact = "Lucia.Segovia-De-La-Revilla@lshtm.ac.uk", 
        supple = "NA") %>% relocate(
          c(fct_year, start_date, end_date), .after = "fct_name") %>% 
          relocate(
            fct_lead_organization, .before = "contact_type") %>% 
            relocate( fct_licence , .after = "keywords") %>% 
            relocate( fct_documentation_link , .after = "supple") %>% 
        rename_all(~geo.col) %>% glimpse()
        
        
y <-   geonetwork %>% mutate_at("Edition", as.character) %>% 
    bind_rows(., x)
    
    
    xlsx::write.xlsx(as.data.frame(y), 
                     file = "metadata/Geonetwork-Metadata.xlsx",
                     sheetName="Sheet1", 
                     col.names=TRUE, row.names=FALSE, append=FALSE)
         


## - Variable names in MAPS FCT - ####
    
var.name <- read.csv(here::here("fct-variable-names.csv"))
names(var.name)

var.name$Column.Name[var.name$Column.Name == "phyticacid_in_mg"] <- "phytate_in_mg"
var.name$Description[var.name$Column.Name == "phyticacid_in_mg"] <-  "The phytate content of this food item, in milligrams per 100g"

write.csv(var.name, here::here("metadata", "fct-variable-names.csv"), row.names = F)
    