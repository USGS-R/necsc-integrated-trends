## FIGURE_lakes_streams_airt_sens_slope

library(lakeattributes)
library(lubridate)
library(lme4)
source('R/sens_seasonal.R')

strmtemps = readRDS('data/temperature_streams/stream_temperature_wqp.rds')

lktemps   = read.csv('data/temperature_lakes/lake_obs.csv', sep=',', header=TRUE, as.is=TRUE)

load('data/temperature_lakes/slope_population.Rdata', verbose=TRUE)

lake_slopes$depth = floor(lake_slopes$depth)

boxplot(slopes~depth, head(lake_slopes, 1e7), ylim=c(-0.5,0.5))
abline(h=0)
