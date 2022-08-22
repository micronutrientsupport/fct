

colnames(ken_genus)[c(1,3)] <- paste0("ke18_", colnames(ken_genus)[c(1,3)])
colnames(wafct.genus)[c(1,3)] <- paste0("wa19_", colnames(wafct.genus)[c(1,3)])

merge(ken_genus, wafct.genus, all = T) %>% 
  write.csv(., here::here("inter-output", "fct_dictionary.csv"), row.names = F)


         