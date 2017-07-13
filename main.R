library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(broom)
library(ggplot2)


sapply(list.files(pattern="[.]R$", path="./class/", full.names=TRUE), source)

geoCrime <- GeoCrime(mapFilePath = "../data/bialystok/bialystok.shp", crimeDataFilePath = "../data/Polska/zdarzenia_rsow.csv")
geoCrime$init()
geoCrime$drawPlot()
