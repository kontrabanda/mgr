library("sp")
library("rgdal")
library(rgeos)
library(ggplot2)

getwd()

poland.map <- readOGR(dsn="./data/Polska/powiaty/powiaty.shp")

class(poland.map)

ncol(poland.map@data)

poland.map@data <- poland.map@data[ , c(6,16)] #weźmy tylko nazwy województw oraz ich powierzchnie
names(poland.map@data) <- c("nazwa", "powierzchnia") 

sum(poland.map@data$powierzchnia)


###########################################################################################################
rsowFile <- "./data/Polska/zdarzenia_rsow.csv"

#eur.sp <- read.csv("http://quantup.pl/dane/blog/straz-pozarna.csv")
rsowData <-read.csv(file = rsowFile, sep = "|")

#filteredRSOWData = rsowData[rsowData$KAT == 'KRA',]
filteredRSOWData = rsowData
latRSOW <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredRSOWData$LAT)))
lngRSOW <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredRSOWData$LNG)))

coords <- data.frame(lngRSOW, latRSOW) #tworzymy df ze wspolrzednymi
names(coords) <- c("x", "y")
data <- data.frame(filteredRSOWData$KAT) #ramka z danymi

poland.sp <- SpatialPointsDataFrame(coords, data, proj4string = CRS(proj4string(poland.map)), match.ID=TRUE)


poland.map <- spTransform(poland.map, CRS("+init=epsg:4326")) #WGS-84
poland.sp <- spTransform(poland.sp, CRS("+init=epsg:4326")) #WGS-84

#int <- gIntersects(poland.sp, poland.map, byid = T) #może to chwilę potrwać

#clipped <- apply(!int, MARGIN = 2, all)

#plot.new()
#plot(poland.map)
#points(poland.sp, col = "green")
#plot(poland.sp)
#points(poland.sp[which(clipped), ])  #wycięte punkty

poland.sp2 <- poland.sp

poland.aggr <- aggregate(x = poland.sp2["filteredRSOWData.KAT"], by = poland.map, FUN = length)

#dodajmy interesujące nas dane do poland.map
poland.map@data$KAT <- poland.aggr@data$filteredRSOWData.KAT
head(poland.map@data, n=4)


EPSG <- make_EPSG()
EPSG[grepl("WGS 84$", EPSG$note), ]

poland.map.gg <- fortify(poland.map, region="nazwa")
head(poland.map.gg, n=2)

poland.map.gg <- merge(poland.map.gg, poland.map@data, by.x="id", by.y="nazwa", sort=FALSE)
head(poland.map.gg, n=2)

map <- ggplot() + geom_polygon(data = poland.map.gg, aes(long, lat, group = group, fill = KAT), colour = "black", lwd=0.1) + ggtitle("Mapa Polski") + labs(x = "E", y = "N", fill = "Liczba\nkradzieży")
map + scale_fill_gradient(low = "white", high = "red")

