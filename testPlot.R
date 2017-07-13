library("sp")
library("rgdal")
library(rgeos)
library(utils)
library(ggplot2)

sapply(list.files(pattern="[.]R$", path="scripts/functions/", full.names=TRUE), source);


rsow.sp <- getCrimePoints("./data/Polska/zdarzenia_rsow.csv", 'LAT', 'LNG', 'KAT')
swd.sp <- getCrimePoints("./data/Polska/zdarzenia_swd.csv", 'GEO_LAT', 'GEO_LONG', 'GRUPA_KOD')

rsowTheft.sp <- getCrimePoints("./data/Polska/zdarzenia_rsow.csv", 'LAT', 'LNG', 'KAT', 'KRA')
swdTheft.sp <- getCrimePoints("./data/Polska/zdarzenia_swd.csv", 'GEO_LAT', 'GEO_LONG', 'GRUPA_KOD', 'KRA')

counties.map <- getBordersSP("./data/Polska/powiaty/powiaty.shp")
commons.map <- getBordersSP("./data/Polska/gminy/gminy.shp")
record.map <- getBordersSP("./data/Polska/ewidencja/obreby_ewidencyjne.shp")

allFlag = T

countiesOverallFlag = T
commonsOverallFlag = T

countiesTheftFlag = T
commonsTheftFlag = T

if(countiesOverallFlag | allFlag) {
  drawAreaDensityPlot(counties.map, rsow.sp, "RSOW Powiaty Ogólnie", "Liczba\nprzestępstw")
  drawAreaDensityPlot(counties.map, swd.sp, "SWD Powiaty Ogólnie", "Liczba\nprzestępstw")  
}

if(commonsOverallFlag | allFlag) {
  drawAreaDensityPlot(commons.map, rsow.sp, "RSOW Gminy Ogólnie", "Liczba\nprzestępstw")
  drawAreaDensityPlot(commons.map, swd.sp, "SWD Gminy Ogólnie", "Liczba\nprzestępstw")  
}

if(countiesTheftFlag | allFlag) {
  drawAreaDensityPlot(counties.map, rsowTheft.sp, "RSOW Powiaty Kradzieże", "Liczba\nkradzieży")
  drawAreaDensityPlot(counties.map, swdTheft.sp, "SWD Powiaty Kradzieże", "Liczba\nkradzieży")  
}

if(commonsTheftFlag | allFlag) {
  drawAreaDensityPlot(commons.map, rsowTheft.sp, "RSOW Gminy Kradzieże", "Liczba\nkradzieży")
  drawAreaDensityPlot(commons.map, swdTheft.sp, "SWD Gminy Kradzieże", "Liczba\nkradzieży")  
}



#drawAreaDensityPlot(record.map, rsow.sp, "RSOW Gminy Ogólnie", "Liczba\nprzestępstw")
