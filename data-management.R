
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

#fct <- c("ke18_dict.R", "wa19_dict.R", "uk21_dict.R", "us19_dict.R", "ja15_dict.R")

#source(here::here("FCTs", fct[1]))

fcts <- list.files(here::here("FCTs") , "*_dict.R")

source(here::here("FCTs", fcts[2]))

eval(parse(text = paste0("source('FCTs/", fcts, "')")))
  
#Saving dictionary-to-FCTs-matches
file <- sort(list.files(here::here("metadata") , "dict_fct_compilation_v\\."),
             decreasing = T)[1]

for(i in 1:length(path)) {
write.csv(read.csv(here::here("metadata", file)),
          paste0(path[i], "/data/",file), row.names = F)
}
