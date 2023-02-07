
#Updating files w/ dictionary data dependencies


#Loading path - untracked file w/ personal WD
source(here::here("path.R"))
source(here::here("MAPS_Dictionary-Protocol.R"))

#Saving dictionary
for(i in 1:length(path)) {
  
saveRDS(dictionary.df, file = paste0(path[i], "/data/dictionary.df.rds"))

}

#lapply(dictionary.df, file = paste0(path, "/data/dictionary.df.rds"), saveRDS)

#Saving dictionary-to-FCTs-matches
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v"),
             decreasing = T)[2]

write.csv(read.csv(here::here("metadata", file)),
          paste0(path, "/data/",file), row.names = F)
        