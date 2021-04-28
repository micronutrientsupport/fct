
library(tidyverse)


fct_metadata <- read.csv(here::here( "fct_metadata.csv"))


fct_metadata$Variable_Name <-  str_replace(fct_metadata$Variable_Name, "in_.g$|in_kcal|in_kj", "method")

fct_metadata %>% mutate(Variable_Name = str_replace(Variable_Name, "_in_.*g$|_in_kcal|_in_kj|_cal",
                                                    "_method"), 
                        Variable_Name = str_replace(Variable_Name, 
                            "vitamina_method", "vitamina_rae_method")) %>% 
  write.csv(here::here("fct_metadata_v1.0.csv"), row.names = FALSE)