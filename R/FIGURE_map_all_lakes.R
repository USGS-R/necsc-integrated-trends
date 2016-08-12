## create an overview map of all lakes
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(ggplot2)
library(broom)

modrun = read.table('data/necsc_model_output/ACCESS_core_metrics.tsv', sep='\t', header=TRUE, as.is=TRUE)

nhd_lakes = readOGR('data/necsc_nhd', layer='NHDWaterbody')
states = map_data('state', region = c('wisconsin', 'minnesota', 'michigan'))

#drop non-modeled lakes
modeled_ids = sapply(unique(modrun$site_id), function(x) substr(x, 5, nchar(x)))

nhd_lakes = nhd_lakes[nhd_lakes@data$Prmnn_I %in% modeled_ids, ]

nhd_lakes@data$id = rownames(nhd_lakes@data)
nhd_lakes.df = tidy(nhd_lakes)
  #nhd_lakes.df = join(nhd_lakes.points, nhd_lakes@data, by="id")

alllakes = ggplot(wisco, aes(long, lat, group=group)) + geom_polygon(fill=rgb(0,0,0,0.5), color='white') + 
  geom_polygon(data=nhd_lakes.df, aes(long, lat, group=group), color='blue', fill='blue', size=0) + 
  guides(color=FALSE, shape=FALSE) + coord_map("conic", lat0 = 30) + theme_bw() + 
  xlab('Longitude') + ylab('Latitude')

ggsave('output/data_paper/all_lake_map.png', plot = alllakes, width=138, units='mm', dpi=450)
ggsave('output/data_paper/all_lake_map.tiff', plot = alllakes, width=138, units='mm', compression='lzw', dpi=450)

