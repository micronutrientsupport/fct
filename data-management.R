
#Updating files w/ dictonary data dependecies


#Loading path - untracked file w/ personal WD
source(here::here("path.R"))

#Saving dictionary
saveRDS(dictionary.df, file = paste0(path, "/data/dictionary.df.rds"))

#Saving dictionary-to-FCTs-matches
file <- list.files(here::here("metadata") , "dict_fct_compilation_v")[1]
write.csv(read.csv(here::here("metadata", file)),
          paste0(path, "/data/",file), row.names = F)
        