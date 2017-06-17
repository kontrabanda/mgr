getBordersSP <- function(fileName)
{
  borders.map <- readOGR(dsn=fileName)
  
  class(borders.map)
  ncol(borders.map@data)
  
  borders.map@data <- borders.map@data[ , c(6,16)] #weźmy tylko nazwy województw oraz ich powierzchnie
  names(borders.map@data) <- c("nazwa", "powierzchnia") 
  
  borders.map <- spTransform(borders.map, CRS("+init=epsg:4326")) #WGS-84
  
  return (borders.map)
}