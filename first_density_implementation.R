library(sp)
library(plotGoogleMaps)
library(spatstat)
library(raster)
library(maptools)
library(plotrix)
library(rgeos)

data <- read.csv("../data/Polska/zdarzenia_rsow.csv", sep = "|")
data <- data[!is.na(data$LAT)&!is.na(data$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
data$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", data[["LAT"]])))
data$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", data[["LNG"]])))
######################################################################

coordinates(data) =~ LNG+LAT

border <- shapefile("../data/bialystok/bialystok.shp")
border <- spTransform(border, CRS("+init=epsg:4326"))

unique(border$jpt_nazwa_)

projection(data) = projection(border)
overlay <- over(data, border)

data$over <- overlay$jpt_nazwa_
data.Bialystok <- data[!is.na(data$over), ]


####################################################################
png('PP_plot.png', width=2500, height=3000, res=300)
plot(data.Bialystok, pch="+", cex=0.8, main="", col=data.Bialystok$KAT)
plot(border, add=T)

legend(x="bottomleft", pch="+", col=unique(data.Bialystok$KAT), legend=unique(data.Bialystok$KAT), cex=0.8)
dev.off()

##############################################################
mean_centerX <- mean(data.Bialystok@coords[,1])
mean_centerY <- mean(data.Bialystok@coords[,2])

standard_deviationX <- sd(data.Bialystok@coords[,1])
standard_deviationY <- sd(data.Bialystok@coords[,2])

standard_distance <- sqrt(sum(((data.Bialystok@coords[,1]-mean_centerX)^2+(data.Bialystok@coords[,2]-mean_centerY)^2))/(nrow(data.Bialystok)))

jpeg("PP_Ellipse.jpeg", width=2500, height=3000, res=300)
plot(data.Bialystok, pch="+", cex=0.8, main="")
plot(border, add=T)
points(mean_centerX, mean_centerY, col="red", pch=16)
draw.ellipse(mean_centerX, mean_centerY, a=standard_deviationX, b=standard_deviationY, border="red", lwd=2)
dev.off()

#############################################################
Crime <- data.Bialystok[data.Bialystok$KAT == unique(data.Bialystok$KAT)[3], ]
Crime <- remove.duplicates(Crime)

BorderUTM <- spTransform(border, CRS("+init=epsg:32630"))
Crime.UTM <- spTransform(Crime, CRS("+init=epsg:32630"))
window <- as.owin(BorderUTM)
Crime.ppp <- ppp(x=Crime.UTM@coords[, 1], y=Crime.UTM@coords[, 2], window=window)

Crime.ppp$n/sum(sapply(slot(BorderUTM, "polygons"), slot, "area"))

jpeg("PP_QuadratCounting.jpeg", width=2500, height=3000, res=300)
plot(Crime.ppp, pch="+", cex=0.5, main="Drugs")
plot(quadratcount(Crime.ppp, nx = 4, ny = 4), add=T, col="blue")
dev.off()

############################################################
Local.Intensity <- data.frame(Borough=factor(), Number=numeric())

for(i in unique(BorderUTM$jpt_nazwa_)){  
  sub.pol <- BorderUTM[BorderUTM$jpt_nazwa_==i, ]  
  sub.ppp <- ppp(x=Crime.ppp$x, y=Crime.ppp$y, window=as.owin(sub.pol))
  Local.Intensity <- rbind(Local.Intensity, data.frame(Borough=factor(i, levels=BorderUTM$jpt_nazwa_), Number=sub.ppp$n))  
}

colorScale <- color.scale(Local.Intensity[order(Local.Intensity[, 2]), 2], color.spec="rgb", extremes=c("green", "red"), alpha=0.8)

jpeg("PP_BoroughCounting.jpeg", 2000, 2000, res=300)
par(mar=c(5, 13 , 4, 2))
barplot(Local.Intensity[order(Local.Intensity[, 2]), 2], names.arg=Local.Intensity[order(Local.Intensity[, 2]), 1], horiz=T, las=2, space=1, col=colorScale)
dev.off()

###########################################################
jpeg("Kernel_Density.jpeg", width=2500, height=3000, res=300)
par(mfrow=c(2, 2))
plot(density.ppp(Crime.ppp, sigma = bw.diggle(Crime.ppp), edge=T), main=paste("h =", round(bw.diggle(Crime.ppp), 2)))
plot(density.ppp(Crime.ppp, sigma = bw.ppl(Crime.ppp), edge=T), main=paste("h =", round(bw.ppl(Crime.ppp), 2)))
plot(density.ppp(Crime.ppp, sigma = bw.scott(Crime.ppp)[2], edge=T),main=paste("h =", round(bw.scott(Crime.ppp)[2], 2)))
plot(density.ppp(Crime.ppp, sigma = bw.scott(Crime.ppp)[1], edge=T),main=paste("h =", round(bw.scott(Crime.ppp)[1], 2)))
dev.off()

###########################################################
jpeg("GFunction.jpeg",2500,2000,res=300)
plot(Gest(Crime.ppp), main="Theft Related Crimes")
dev.off()

