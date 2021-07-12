

#install.packages("tabulizer")

library(tabulizer)
library(tidyverse)

t <-  "https://dl.tufts.edu/downloads/g158bw806?filename=d217r336d.pdf"

mwi_table <- extract_tables("https://dl.tufts.edu/downloads/g158bw806?filename=d217r336d.pdf",
                            output = "data.frame",
                            pages = c(21:27), 
                            guess = FALSE)

mwi_table <- extract_tables(t,
                            output = "data.frame",
                            pages = 21)


mwi_table_clean <- reduce(mwi_table, bind_rows)

mwi_table <- extract_tables(t,
method = "decide", 
output = "data.frame")


mwi_table %>% 
pluck(13) %>% 
  as_tibble() %>% head() %>% knitr::kable()



mwi_raw_tbl <- mwi_table %>% 
  pluck(12) %>% 
  as_tibble()

# Show first 6 rows
mwi_table_clean %>% head() %>% knitr::kable()

# Get column names from Row 1
col_names <- mwi_table_clean %>% 
  slice(1) %>%  pivot_longer(cols = everything()) %>%
  mutate(value = ifelse(is.na(value), "Missing", value)) %>%
  pull(value)

mwi_table_clean_rename <- mwi_table_clean %>%
  set_names(col_names) %>%
  slice(-1)

# Show first 6 rows
mwi_table_clean_rename %>% head() %>% knitr::kable()


#    mwi_table_clean %>% filter(str_detect(X, "MW")) 
#    
#    
#    f <- str_which(mwi_table_clean$X, "MW")
#    
#    mwi <- as.data.frame(1:69)
#    
#    for(i in 1:2){
#    
#      f <- f+1
#    
#     mwi[[i]] <- mwi_table_clean %>% slice(f)
#    
#    }
#


#This loop is working :)

mwi <- list()

for(i in 1:3){
  
  f <- str_which(mwi_table_clean$X, "MW") %>% as.numeric()
  
  f <- f+i-1
  
  mwi[[i]] <- mwi_table_clean %>% slice(f)
  
  print(i)
  
}


mwi_clean <- reduce(mwi, bind_cols)





mwi_clean %>% relocate(starts_with("GROUP.1"), .after = "X...1") %>% 
  mutate(water = str_extract(GROUP.1..STAPLES...2, "[:digit:]{1,2}\\.[:digit:]{1,2}"),
         water.low = str_detect(GROUP.1..STAPLES...2, "\\[*\\]"))

compo <- "[:digit:]{1,3}\\.[:digit:]{1,2}"

bracket <- "\\[.*?\\]"

all <- "[:digit:]{1,3}\\.[:digit:]{1,2}|\\[.[:digit:]{1,3}\\.[:digit:]{1,2}\\]"

id <- "MW[:digit:]{2}\\_[:digit:]{4}"

food.group <- "[:upper:]{1}[:lower:]{2,}"

ref <- "R?[:digit:]{1,2}|[:upper:]{2,4}"

mwi_clean %>% relocate(starts_with("GROUP.1"), .after = "X...1") %>% 
  mutate(water = str_extract(GROUP.1..STAPLES...2, compo),
         water.low = str_detect(GROUP.1..STAPLES...2, bracket), 
         food.description = str_replace(GROUP.1..STAPLES...2, 
                                  all, "")) %>% 
  mutate(fibre = str_extract(GROUP.1..STAPLES...16, compo),
         fibre.low = str_detect(GROUP.1..STAPLES...16, bracket), 
         food.description2 = str_replace_all(GROUP.1..STAPLES...16, all, ""), 
         fibre.low = ifelse(is.na(fibre), str_detect(food.description, bracket), fibre.low),
         fibre = ifelse(is.na(fibre), str_extract(food.description, compo), fibre))

  
mwi_clean %>% relocate(starts_with("X..."), before = "GROUP.1..STAPLES...2") %>% 
  mutate(code = str_extract(X...1, id), 
         food.group = ifelse(str_detect(X...15, food.group), 
                                        X...15, str_extract(X...1, food.group)),
         ref = ifelse(str_detect(X...29, ref),X...29, 
                      str_extract(X...15, ref)))
