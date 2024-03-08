

# Load libraries ----
library(dplyr)
library(ggplot2)
library(paletteer) # visualisation - ggplot2 add-on colours
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Loading data ----
## Specify the hces ----
hces <- "ihs4"
# Getting the most recent version of the food consumption file
file_name <- sort(list.files(here::here("inter-output"), 
                             paste0("food-cons_", hces)), decreasing = TRUE)[1]

# Getting the most recent version
data <- readRDS(here::here("inter-output", file_name)) %>% 
  filter(!is.na(as.numeric(g_consumed)))

# data <- read.csv(here::here("data", "hces", "app_cons_clean.csv"))
# names(data)
# 
# fcode <- read.csv(here::here("inter-output", "hces", "nga5-item-code.csv")) %>% 
#   rename(item_code = "item_cd_code", item = "item_cd_name")
# 
# names(fcode)


# Getting afe (ihs4)
afe <- read.csv(here::here("data", "hces", paste0(hces, ".afe_V1.0.0.csv")))

data <- data %>% left_join(., afe) %>% 
  mutate(afe_consumed = g_consumed/afe)

# Getting roster household data
hh_info <- read.csv(here::here("inter-output", "hces",
                               paste0(hces,".hh.csv")))

# Boundaries ----
# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this

# Country
mwi_bnd  <- st_read(here::here( "data",
                                 "boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm0_nso_hotosm_20230329.shp"))

mwi_bnd <- st_make_valid(mwi_bnd) # Check this


# Joining the data on consumption to hh info
data <-  data %>% left_join(., hh_info)

# data <- left_join(data, fcode)
# data$item_code <- as.character(data$item_code)
# 
# table(data$consYN)
# 
# data <- data %>% filter(consYN == "1") 
# 
# data %>% filter(item_code == "79") %>% count()
# 
# data %>% filter(grepl("[[:alpha:]]", item_oth) & item_code == "79") %>% count()
#                 
# data %>% filter(grepl("[[:alpha:]]", item_oth) & item_code == "79")

n <- 10
top <- data %>% count(item_code) %>% arrange(desc(n)) %>% .[c(1:n),1]

top_cons <- data %>% group_by(item_code) %>% 
  summarise(med = median(afe_consumed, na.rm=TRUE)) %>% 
  arrange(desc(med)) %>% .[c(1:n),1] %>% pull()


data %>% filter(item_code %in% top_cons #& app_cons < 500
                ) %>% 
  ggplot(aes(reorder(item, afe_consumed , na.rm = TRUE), afe_consumed)) +
  geom_boxplot() +
  theme_light() +
  labs( x = "", 
        y = "g/ AFE/ day",
        title = "Top 10 food consumed in Malawi (IHS4)") +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  coord_flip()

# Graphics

title_name <- "Maize consumed in Malawi (IHS4)"

data %>% 
  filter(grepl("maize", item, ignore.case=TRUE)) %>% 
  #filter(grepl("fish", item, ignore.case=TRUE)) %>% 
  #filter(grepl("fre", item, ignore.case=TRUE)) %>% 
  ggplot(aes(reorder(item, afe_consumed , na.rm = TRUE), afe_consumed)) +
  geom_boxplot() + theme_light() +
  labs( x = "", 
        y = "g/ AFE/ day",
        title = title_name) +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  coord_flip()

Se <- 0.01687*(100-13)/100
Se.25 <- 0.00857*(100-13)/100
Se.75 <- 0.0361*(100-13)/100
Se.mwi <- 1.61271/100
Se.ke <- 8/100

n.size <- 18
n2.size <- 20

# Custom legend colour & labels
col_break <- c("1" = "#00BFC4", "2" = "#F8766D")
col_labels <- c("1" = "Urban", "2" = "Rural")

col_labels <- c(
"Se_25" = "GeoNut Lowest quantile (.25)",
"Se_med" = "GeoNut Median",
"Se_mwi" = "Malawi FCT",
"Se_75" = "GeoNut Highest quantile (.75)",
"Se_ke" =  "Kenya FCT")

#scale_colour_manual(values =  col_break,
#                    # breaks = col_break,
#                    labels = col_labels) +

#calecopal::superbloom1

data %>% 
  filter(grepl("maize", item, ignore.case=TRUE)) %>% 
  mutate(Se_med = afe_consumed*Se, 
         Se_25 = afe_consumed*Se.25, 
         Se_75 = afe_consumed*Se.75,
         Se_mwi = afe_consumed*Se.mwi,
         Se_ke = afe_consumed*Se.ke
         ) %>%
  group_by(HHID) %>% 
  summarise_at(vars(starts_with("Se_")), sum) %>% 
 pivot_longer(cols = starts_with("Se_"),
               names_to = "Se_scenario", 
               values_to = "Se_cons") %>% 
  ggplot(aes(Se_cons, fill = Se_scenario)) + 
  geom_density(alpha = 0.6) +
  labs(
  #  title = "Daily Se intake from maize in Malawi",
    y = "",
    x = "\n mcg/day"
  ) +
  scale_fill_paletteer_d("calecopal::fire",
  name = "Maize Se values", 
  limits = c("Se_25",
           "Se_med",
           "Se_mwi",
           "Se_75",
           "Se_ke"),
      labels = col_labels
 # option = "D"
  ) +
  theme_minimal() +
  theme(
   # title = element_text(size = n2.size),
    axis.text.x = element_text(size = n.size),
    axis.title.x = element_text(size = n.size),
    axis.text.y  = element_text(size = n.size),
    legend.text =  element_text(size = n.size),
    legend.title =  element_text(size = n.size),
    legend.position = c(.7, .8) 
  ) 


geo.data <-  data %>% filter(!is.na(lat_modified)) %>% 
  st_as_sf(., coords =c( "lon_modified", "lat_modified"),
           crs = "EPSG:4326") 

# 
# tm_shape(geo.data) +
#   tm_symbols(col="total", size = .5) +
#    tm_layout(legend.outside = TRUE)
  
geo.data  <-  st_join(geo.data, dist_bnd)



geo.data %>%  st_drop_geometry() %>% 
  filter(grepl("fish", item, ignore.case=TRUE)) %>% 
  filter(grepl("fre", item, ignore.case=TRUE)) %>% 
  group_by(HHID, ADM2_EN) %>% summarise(total = sum(afe_consumed, na.rm = TRUE)) %>% 
  group_by(ADM2_EN) %>% summarise(mean = mean(total), sd = sd(total)) %>% 
  right_join(., dist_bnd %>% select(ADM2_EN, geometry)) %>% 
  st_sf() %>% 
  tm_shape() +
  tm_polygons(col="mean") +
 # tm_polygons(col = "sd") +
 tm_layout(legend.outside = TRUE) +  
  tm_shape(dist_bnd) +
  tm_borders(col = "#666666", alpha = 0.6, lwd = 0.5) 
