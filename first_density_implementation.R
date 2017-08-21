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


