
td = read.csv('data/trends/lakes_gltc_jas.csv', as.is=TRUE)

png('output/trend_variability_output/gltc_trend_uncertainty.png', res=450, height=5000, width=3000)

dotchart(td$trend, labels=td$Lake.name, cex=0.5, xlim=c(-0.15,0.15))
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
