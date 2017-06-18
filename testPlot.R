library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(ggplot2)

sapply(list.files(pattern="[.]R$", path="scripts/functions/", full.names=TRUE), source);


#poland.sp <- getCrimePoints("./data/Polska/zdarzenia_rsow.csv", 'LAT', 'LNG', 'KAT')
poland.sp <- getCrimePoints("./data/Polska/zdarzenia_swd.csv", 'GEO_LAT', 'GEO_LONG', 'GRUPA_KOD')
poland.map <- getBordersSP("./data/Polska/powiaty/powiaty.shp")

drawAreaDensityPlot(poland.map, poland.sp, "Mapa Polski", "Liczba\nprzestępstw")

