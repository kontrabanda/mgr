library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(ggplot2)

sapply(list.files(pattern="[.]R$", path="scripts/functions/", full.names=TRUE), source);

###########################################################################################################

poland.sp <- getCrimePoints("./data/Polska/zdarzenia_rsow.csv")
poland.map <- getBordersSP("./data/Polska/powiaty/powiaty.shp")

poland.sp2 <- poland.sp

poland.aggr <- aggregate(x = poland.sp2["filteredCrimeData.KAT"], by = poland.map, FUN = length)

#dodajmy interesujące nas dane do poland.map
poland.map@data$KAT <- poland.aggr@data$filteredCrimeData.KAT
head(poland.map@data, n=4)


EPSG <- make_EPSG()
EPSG[grepl("WGS 84$", EPSG$note), ]

poland.map.gg <- fortify(poland.map, region="nazwa")
head(poland.map.gg, n=2)

poland.map.gg <- merge(poland.map.gg, poland.map@data, by.x="id", by.y="nazwa", sort=FALSE)
head(poland.map.gg, n=2)

map <- ggplot() + geom_polygon(data = poland.map.gg, aes(long, lat, group = group, fill = KAT), colour = "black", lwd=0.1) + ggtitle("Mapa Polski") + labs(x = "E", y = "N", fill = "Liczba\nkradzieży")
map + scale_fill_gradient(low = "white", high = "red")

