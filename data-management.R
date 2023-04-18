
#Updating files w/ dictionary data dependencies


#Loading path - untracked file w/ personal WD
source(here::here("path.R"))
source(here::here("MAPS_Dictionary-Protocol.R"))

#Saving dictionary
for(i in 1:length(path)) {
  
saveRDS(dictionary.df, file = paste0(path[i], "/data/dictionary.df.rds"))

}

#lapply(dictionary.df, file = paste0(path, "/data/dictionary.df.rds"), saveRDS)

# Run source() 

fct <- c("kenfct.R", "wafct.R", "fct_uk.R", "fct_usda.R", "fct_ja15.R")

source(here::here("FCTs", fct[1]))

eval(parse(text = paste0("source('FCTs/", fct, "')")))
  
#Saving dictionary-to-FCTs-matches
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

for(i in 1:length(path)) {
write.csv(read.csv(here::here("metadata", file)),
          paste0(path[i], "/data/",file), row.names = F)
}