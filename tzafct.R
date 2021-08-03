
library(tabulizer)
library(shiny)
library(miniUI)
library(tidyverse)

#Tanzania

t <- "https://cdn1.sph.harvard.edu/wp-content/uploads/sites/30/2012/10/tanzania-food-composition-tables.pdf"

p <- locate_areas(t, pages = c(10:30))


extract_tables(t,
               output = "data.frame",
               pages = 18,
               area = list(c(416, 55, 610, 482)),
               guess = F) %>% as.data.frame()