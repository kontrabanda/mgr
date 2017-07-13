getCrimePoints <- function(fileName, latName, lngName, categoryName, categoryFilter = NULL) {
  crimeData <-read.csv(file = fileName, sep = "|")
  
  #filteredCrimeData = crimeData[crimeData$KAT == 'KRA',]
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
  return (crime.sp)
}