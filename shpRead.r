library(rgdal)
library(ggplot2)

shpFile <- "./data/Polska/woj/województwa.shp"
rsowFile <- "./data/Polska/zdarzenia_rsow.csv"
swdFile <- "./data/Polska/zdarzenia_swd.csv"

rsowData = read.csv(file = rsowFile, sep = "|")
swdData = read.csv(file = swdFile, sep = "|")
filteredRSOWData = rsowData[rsowData$KAT == 'KRA',]
filteredSWDData = swdData[swdData$GRUPA_KOD == 'BOJ',]

options(digits=9)
latRSOW <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredRSOWData$LAT)))
lngRSOW <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredRSOWData$LNG)))
kat <- as.character(filteredRSOWData$KAT)

latSWD <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredSWDData$GEO_LAT)))
lngSWD <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredSWDData$GEO_LONG)))

boundires <- readOGR(shpFile)
boundires <- spTransform(boundires, CRS("+init=epsg:4267"))

head(boundires, n=2)

xRange <- range(c(14, 25))
yRange <- range(c(49, 55))

plot(boundires, axes = TRUE, ylim=yRange, xlim=xRange)
par(new=TRUE)
plot(lngRSOW, latRSOW, col = 'red', ylim=yRange, xlim=xRange)
par(new=TRUE)
plot(lngSWD, latSWD, pch = 1, cex = 1 , col = 'blue', ylim=yRange, xlim=xRange)
