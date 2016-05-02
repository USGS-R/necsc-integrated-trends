library(laketemps)
library(randomForest)
library(zyp)

all_lakes = laketemps::get_lake_names()

all_wtr = lapply(all_lakes, get_surface_temps, type='Lake.Temp.Summer.InSitu')
bad_i = sapply(all_wtr, function(x) nrow(x) == 0)


for(i in 1:length(all_wtr)){
  if(nrow(all_wtr[[i]]) > 0){
    all_wtr[[i]]$lakeid = all_lakes[i]
  }
  
}

wtr_long = do.call(rbind, all_wtr)

#wtr_long$lakeid = as.factor(wtr_long$lakeid)
#contrasts(wtr_long$lakeid) = contr.helmert(length(unique(wtr_long$lakeid)))



#intercepts are definitely different. Trends? (once intercepts accounted for) Not so much
summary(aov(Lake.Temp.Summer.InSitu~year+lakeid, data=wtr_long))
summary(aov(Lake.Temp.Summer.InSitu~year*lakeid, data=wtr_long))

#more parsimonious model wins
anova(aov(Lake.Temp.Summer.InSitu~year*lakeid, data=wtr_long),aov(Lake.Temp.Summer.InSitu~year+lakeid, data=wtr_long))



lakenames = unique(wtr_long$lakeid)
nyear = rep(NA, length(lakenames))
cis = list()
trend = rep(NA, length(lakenames))
for(i in 1:length(lakenames)){
  tmp = subset(wtr_long, lakeid == lakenames[i])
  if(nrow(tmp) < 13){ #gltc cutoff
    cis[[i]] = matrix(c(NA, NA, NA,NA), nrow=2)
    next
  }
  trend[i] = zyp.sen(Lake.Temp.Summer.InSitu~year, data=tmp)$coeff[2]
  
  cis[[i]] = confint(zyp.sen(Lake.Temp.Summer.InSitu~year, data=tmp))
  nyear[i] = nrow(tmp)
}

lows = sapply(cis, function(x) x[2,1])
highs = sapply(cis, function(x) x[2,2])
ranges = highs-lows

td = data.frame(lakename=lakenames, trend, low=lows, high=highs, range=ranges)
td = na.omit(td[order(td$trend),])


png('output/trend_variability_output/gltc_trends_uncertainties.png', res=450, height=5000, width=3000)

dotchart(td$trend, labels=td$lakename, cex=0.5, xlim=c(-0.15,0.15))
medtrend = median(td$trend, na.rm=TRUE)
for(i in 1:nrow(td)){
  lines(c(max(td$low[i], -0.16),td$high[i]),c(i,i))
  
  if(medtrend < td$low[i] || medtrend > td$high[i]){
    lines(c(td$low[i],td$high[i]),c(i,i), lwd=2)
    #points(td$trend[i], i)
  }
}

abline(v=medtrend)

dev.off()
