#FIUGURE_wqp_lake_stream_sens

#all_slopes
#lake_slopes
lake_slopes = subset(lake_slopes, depth < 2)

tmp = data.frame(slopes=all_slopes$slopes)
tmp$type = 'stream'
tmp.lakes = data.frame(slopes=lake_slopes$slopes, type=rep('lake', nrow(lake_slopes)))
tmp = rbind(tmp, tmp.lakes)


boxplot(slopes~type, tmp, ylim=c(-0.2, 0.2))

laketrends   = ddply(lake_slopes, 'site_id', function(df){median(df$slopes, na.rm=TRUE)})
streamtrends = ddply(all_slopes, 'sites_i', function(df){median(df$slopes, na.rm=TRUE)})


plot(c(1,2), c(median(laketrends$V1), median(streamtrends$V1)), ylim=c(-0.04, 0.03))

points(rep(1, nrow(laketrends)), laketrends$V1, pch=16, col=rgb(0,1,0.1))

lines(c(1,1), )