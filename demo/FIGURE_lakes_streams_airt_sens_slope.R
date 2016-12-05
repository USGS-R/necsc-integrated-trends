## FIGURE_lakes_streams_airt_sens_slope

library(lakeattributes)
library(lubridate)
library(lme4)
source('R/sens_seasonal.R')

strmtemps = readRDS('data/temperature_streams/stream_temperature_wqp.rds')

data(wtemp)
zmax = ddply(zmax, 'site_id', function(df){data.frame(zmax=max(df$zmax_m))})

lktemps = merge(wtemp, zmax, by='site_id')

lktemps = subset(lktemps, depth <= zmax)



lake_slopes$depth = floor(lake_slopes$depth)

boxplot(slopes~depth, head(lake_slopes, 1e7), ylim=c(-0.5,0.5))
abline(h=0)
