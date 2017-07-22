library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(broom)
library(ggplot2)
library(plyr)


sapply(list.files(pattern="[.]R$", path="./class/", full.names=TRUE), source)

geoCrime <- GeoCrime(mapFilePath = "../data/bialystok/bialystok.shp", crimeDataFilePath = "../data/Polska/zdarzenia_rsow.csv")
geoCrime$init()
geoCrime$drawPlot()

head(geoCrime$crimeAggregateData)
unique(geoCrime$crimeAggregateData$category)

ggplot() + 
  geom_polygon(data = geoCrime$crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) +
  geom_point(data = data.frame(geoCrime$crimeInRegionDataSP), aes(x, y), colour = "green", size=2, alpha=1/4)



colours <- geoCrime$getCrimeCategoryColour()
data <- data.frame(geoCrime$crimeInRegionDataSP)

data$colours <- colours

head(colours)

ggplot() + 
  geom_polygon(data = geoCrime$crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) +
  geom_point(data = data, aes(x, y), colour = colours, size=2, alpha=1/4)


