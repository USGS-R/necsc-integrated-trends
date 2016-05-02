
library(laketemps)


all_lakes = laketemps::get_lake_names()

all_wtr = lapply(all_lakes, get_surface_temps, type='Lake.Temp.Summer.InSitu')
bad_i = sapply(all_wtr, function(x) nrow(x) == 0)


for(i in 1:length(all_wtr)){
  if(nrow(all_wtr[[i]]) > 0){
    all_wtr[[i]]$lakeid = all_lakes[i]
  }
}

wtr_long = do.call(rbind, all_wtr)


lakenames = unique(wtr_long$lakeid)
nyear = rep(NA, length(lakenames))
cis = list()
for(i in 1:length(lakenames)){
  tmp = subset(wtr_long, lakeid == lakenames[i])
  
  cis[[i]] = confint(lm(Lake.Temp.Summer.InSitu~year, data=tmp))
  nyear[i] = nrow(tmp)
}

lows = sapply(cis, function(x) x[2,1])
highs = sapply(cis, function(x) x[2,2])
ranges = highs-lows

png('output/trend_variability_output/n_years_vs_confint_range.png', res=300, width=1500, height=1500)

plot(nyear[nyear >= 13], ranges[nyear >= 13], xlim=c(13,26), xlab='N observed years', ylab='', pch=16, col=rgb(0,0,0,0.5))
mtext(text=expression(Trend~confidence~interval~range~("°C"~yr^-1)), side=2, line = 2.7)
dev.off()
