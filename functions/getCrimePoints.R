getCrimePoints <- function(fileName) {
  crimeData <-read.csv(file = fileName, sep = "|")
  
  #filteredCrimeData = crimeData[crimeData$KAT == 'KRA',]
  filteredCrimeData = crimeData
  lat <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData$LAT)))
  lng <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData$LNG)))
  
  coords <- data.frame(lng, lat)
  names(coords) <- c("x", "y")
  data <- data.frame(filteredCrimeData$KAT) 
  
  crime.sp <- SpatialPointsDataFrame(coords, data, proj4string = CRS("+init=epsg:4326"), match.ID=TRUE)
  return (crime.sp)
}