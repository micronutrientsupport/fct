#
#
#   This scripts add specific gravity values (density) for alc. beverage
#    in UK21 for use in the measurement units conversion
#
#
#
##############################################################################



# Adding the gravity & source values
Output_table$specific_gravity[Output_table$fdc_id == "17-506"] <-  1.008  
Output_table$specific_gravity[Output_table$fdc_id == "17-208"] <-  1.018  
Output_table$specific_gravity[Output_table$fdc_id == "17-748"] <-  1.018  
Output_table$specific_gravity[Output_table$fdc_id == "17-223"] <-  1.007  
Output_table$specific_gravity[Output_table$fdc_id == "17-793"] <-  1.093  
Output_table$specific_gravity[Output_table$fdc_id == "17-212"] <-  1.00
Output_table$specific_gravity[Output_table$fdc_id == "17-676"] <-  1.00
Output_table$specific_gravity[Output_table$fdc_id == "17-750"] <-  1.005  
Output_table$specific_gravity[Output_table$fdc_id == "17-751"] <-  1.00
Output_table$specific_gravity[Output_table$fdc_id == "17-768"] <-  1.005  
Output_table$specific_gravity[Output_table$fdc_id == "17-244"] <-  1.05 
Output_table$specific_gravity[Output_table$fdc_id == "17-245"] <-  0.98 
Output_table$specific_gravity[Output_table$fdc_id == "17-758"] <-  1.09
Output_table$specific_gravity[Output_table$fdc_id == "17-821"] <-  1.06
Output_table$specific_gravity[Output_table$fdc_id == "17-767"] <-   1.00
Output_table$specific_gravity[Output_table$fdc_id == "17-812"] <-  1.046  


comment_density <- c("INFOODS:density_v2(2012)(Beer, type bitter or brown ale)", 
  "INFOODS:density_v2(2012)(Beer, strong ale)",
  "INFOODS:density_v2(2012)(Beer, strong ale)", 
  "INFOODS:density_v2(2012)(Cider, dry)", 
  "INFOODS:density_v2(2012)(Liqueur, type advocaat)" ,
  "INFOODS:density_v2(2012)(Beer, light)", 
  "INFOODS:density_v2(2012)(Beer, light)", 
  "INFOODS:density_v2(2012)(Beer, lager)", 
  "INFOODS:density_v2(2012)(Beer, light)", 
  "INFOODS:density_v2(2012)(Beer, lager)", 
  "INFOODS:density_v2(2012)(Bailey's irish cream)", 
  "INFOODS:density_v2(2012)(Tuaca)", 
  "INFOODS:density_v2(2012)(Tia Maria)", 
  "INFOODS:density_v2(2012)(Campari)", 
  "INFOODS:density_v2(2012)(Beer, light)", 
  "INFOODS:density_v2(2012)(Vermouth, sweet)")
  

for(i in 1:length(list_foods)){
  
  comment_new <- paste(Output_table$comments[Output_table$fdc_id%in% list_foods[i]], ",", comment_density[i])
Output_table$comments[Output_table$fdc_id %in% list_foods[i]] <- comment_new

}
