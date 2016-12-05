##OUTPUT_data_release_shapefile
library(rgdal)
library(mda.lakes)
library(lakeattributes)
source(system.file('demo/common_running_functions.R', package='mda.lakes'))


modrun = read.table('data/necsc_model_output/ACCESS_thermal_metrics.tsv', sep='\t', header=TRUE, as.is=TRUE)

nhd_lakes = readOGR('data/necsc_nhd', layer='NHDWaterbody')

#subset shapes to be just modeled lakes
modeled_ids = sapply(unique(modrun$site_id), function(x) substr(x, 5, nchar(x)))
nhd_lakes = nhd_lakes[nhd_lakes@data$Prmnn_I %in% modeled_ids, ]

meta_table = nhd_lakes@data

for(i in 1:nrow(meta_table)){
  
  #add the "lakeid" we are using
  meta_table$lakeid[i] = paste0('nhd_', meta_table$Prmnn_I[i])
  
  # #kd
  # meta_table$kd[i] = secchi_standard(meta_table$lakeid[i])
  # 
  # #wind sheltering
  # meta_table$canht[i] = getCanopy(meta_table$lakeid[i])
  # 
  # #zmax
  # meta_table$zmax[i] = get_zmax(meta_table$lakeid[i])
}


nhd_lakes@data = meta_table[, c('lakeid', 'Prmnn_I', 'GNIS_ID', 'GNIS_Nm', 'ReachCd', 'FType', 'FCode' )]

writeOGR(nhd_lakes, 'data/necsc_nhd', layer = 'model_lakes', driver = "ESRI Shapefile")
