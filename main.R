library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(broom)
library(ggplot2)

fileName <- "../data/Polska/bialystok/pl011l_bialystok.shp"
#fileName <- "../data/miasta/miejscowosci_point.shp"

bordersMap <- readOGR(dsn=fileName)

summary(bordersMap)

bordersDataFrame <- fortify(bordersMap)

ggplot() + geom_polygon(data = bordersDataFrame, aes(long, lat, group = group), 
                        colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)

#######################################################

districtMap <- readOGR("../data/Polska/woj/województwa.shp")
districtDataFrame <- tidy(districtMap)
districtDataFrame <- fortify(districtMap, region="nazwa")

ggplot() + geom_polygon(data = districtDataFrame, aes(long, lat, group = group), 
                        colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)
ggsave(paste(".", "test"), device = "png")

######################################################

comMap <- readOGR("../data/Polska/jednostki_ewidencyjne/jednostki_ewidencyjne.shp")
comDataFrame <- tidy(comMap)
#districtDataFrame <- fortify(districtMap, region="nazwa")

summary(comMap)

ggplot() + geom_polygon(data = districtDataFrame, aes(long, lat, group = group), 
                        colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)
ggsave(paste(".", "test"), device = "png")

######################################################
comMap <- readOGR("../data/Polska/gminy/gminy.shp")
comMap <- comMap[comMap$jpt_kod_je == 2002013 | 
                   comMap$jpt_kod_je == 2002014 |
                   comMap$jpt_kod_je == 2002015 |
                   comMap$jpt_kod_je == 2002023 |
                   comMap$jpt_kod_je == 2002024 |
                   comMap$jpt_kod_je == 2002025 |
                   comMap$jpt_kod_je == 2002032 |
                   comMap$jpt_kod_je == 2002042 |
                   comMap$jpt_kod_je == 2002052 |
                   comMap$jpt_kod_je == 2002152 |
                   comMap$jpt_kod_je == 2002063 |
                   comMap$jpt_kod_je == 2002064 |
                   comMap$jpt_kod_je == 2002065 |
                   comMap$jpt_kod_je == 2002073 |
                   comMap$jpt_kod_je == 2002074 |
                   comMap$jpt_kod_je == 2002075 |
                   comMap$jpt_kod_je == 2002082 |
                   comMap$jpt_kod_je == 2002093 |
                   comMap$jpt_kod_je == 2002094 |
                   comMap$jpt_kod_je == 2002095 |
                   comMap$jpt_kod_je == 2002103 |
                   comMap$jpt_kod_je == 2002104 |
                   comMap$jpt_kod_je == 2002105 |
                   comMap$jpt_kod_je == 2002112 |
                   comMap$jpt_kod_je == 2002123 |
                   comMap$jpt_kod_je == 2002124 |
                   comMap$jpt_kod_je == 2002125 |
                   comMap$jpt_kod_je == 2002133 |
                   comMap$jpt_kod_je == 2002134 |
                   comMap$jpt_kod_je == 2002135 |
                   comMap$jpt_kod_je == 2002143 |
                   comMap$jpt_kod_je == 2002144 |
                   comMap$jpt_kod_je == 2002145 |
                   comMap$jpt_kod_je == 2061011 |
                   comMap$jpt_kod_je == 2002152,]


comMap <- readOGR("../data/bialystok/bialystok.shp")
comMap@data <- comMap@data[ , c(6,16)]
names(comMap@data) <- c("nazwa", "powierzchnia") 

comDataFrame <- tidy(comMap)

crimeData <-read.csv(file = "../data/Polska/zdarzenia_rsow.csv", sep = "|")

latName <- 'LAT'
lngName <- 'LNG' 
categoryName <- 'KAT'

filteredCrimeData = crimeData[ which(crimeData[[latName]] != '' & crimeData[[lngName]] != ''), ]

if(!is.null(categoryFilter)) {
  filteredCrimeData = filteredCrimeData[ which(filteredCrimeData[[categoryName]] != categoryFilter), ]
}

lat <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[latName]])))
lng <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[lngName]])))
category <- filteredCrimeData[[categoryName]]

coords <- data.frame(lng, lat)
names(coords) <- c("x", "y")
data <- data.frame(category)

crime.sp <- SpatialPointsDataFrame(coords, data, proj4string = CRS("+init=epsg:4326"), match.ID=TRUE)

comMap <- spTransform(comMap, CRS("+init=epsg:4326"))

int <- gIntersects(crime.sp, comMap, byid = T) #może to chwilę potrwać
clipped <- apply(!int, MARGIN = 2, all)

crime.sp2 <- crime.sp[which(!clipped), ]

aggr <- aggregate(x = crime.sp2["category"], by = comMap, FUN = length)

comMap@data$category <- aggr@data$category

map.gg <- fortify(comMap, region="nazwa")
map.gg <- merge(map.gg, comMap@data, by.x="id", by.y="nazwa", sort=FALSE)

ggplot() + geom_polygon(data = map.gg, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1)

ggplot() + geom_polygon(data = comMap, aes(long, lat, group = group), 
                        colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)
