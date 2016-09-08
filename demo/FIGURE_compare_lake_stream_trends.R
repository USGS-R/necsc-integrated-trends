#FIGURE_compare_lake_stream_trends

library(maps)

lakes = read.csv('data/trends/lakes_gltc_jas.csv', as.is=TRUE)
streams = read.csv('data/trends/streams_gltc_jas.csv', as.is=TRUE)

lcol = rgb(0,1,0,0.5)
scol = rgb(0,0,1,0.5)



png('output/lake.vs.stream.scatter.png', res=300, width=2100, height=1600)
#par(mfrow=c(2,1), mar=c(0,4,1,1), oma=c(4,0,0,0))

#scatter plot
plot(lakes$sen_airt, lakes$trend, col=lcol, pch=16, ylim=c(-0.25,0.25), xlim=c(-0.15,0.15), xlab='', ylab='')
points(streams$sen_airt, streams$sen_wtr, col=scol, pch=16)

legend('bottomleft', pch=16, col=c(lcol, scol), legend = c('Lakes', 'Streams'))

mtext(expression(Water~temp~trend("°C"~yr^-1)), side=2, line=2.5)
mtext(expression(Air~temp~trend("°C"~yr^-1)), side = 1, line=3)
#abline(0,1)
abline(h=0, lty=3)
abline(v=0, lty=3)

summary(lm(trend~sen_airt, data=lakes))  #r squared like 0.14
summary(lm(sen_wtr~sen_airt, data=streams)) #r squared like 0.001

abline(lm(trend~sen_airt, data=lakes), col=lcol)
abline(lm(sen_wtr~sen_airt, data=streams), col=scol)

dev.off()

##Lake and stream trend density plots

png('output/lake.vs.stream.density.png', res=300, width=2100, height=1600)

ld <- density(lakes$trend)
ld$y = ld$y/nrow(lakes)
sd <- density(streams$sen_wtr)
sd$y = sd$y/nrow(streams)
plot(sd, col='blue', xlab='', ylab='', main='', xlim=c(-0.15,0.15), ylim=c(0,0.5))
lines(ld, col='green')
polygon(ld, col=lcol)
polygon(sd, col=scol)

abline(v=median(lakes$trend))
abline(v=median(streams$sen_wtr))

legend('topleft', fill=c(lcol, scol), legend = c('Lakes', 'Streams'))
mtext(expression(Water~temp~trend("°C"~yr^-1)), side=1, line=3)
mtext('Relative frequency', side=2, line=2.5)

dev.off()


## Lake and stream map 
png('output/lake.vs.stream.map.png', res=300, width=2100, height=1600)

map('usa')

points(lakes$longitude, lakes$latitude, col=lcol, pch=16)
points(streams$dec_long_va, streams$dec_lat_va, pch=16, col=scol)

legend('bottomleft', pch=16, col=c(lcol, scol), legend = c('Lakes', 'Streams'))

dev.off()

