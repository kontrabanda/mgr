getBordersSP <- function(fileName)
{
  borders.map <- readOGR(dsn=fileName)
  
  class(borders.map)
  ncol(borders.map@data)
  
  borders.map@data <- borders.map@data[ , c(6,16)]
  names(borders.map@data) <- c("nazwa", "powierzchnia") 
  
  borders.map <- spTransform(borders.map, CRS("+init=epsg:4326"))
  
  return (borders.map)
}