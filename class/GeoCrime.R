GeoCrime <- setRefClass("GeoCrime",
 fields = list(
   mapFilePath = "character",
   mapDataSP = "SpatialPolygonsDataFrame",
   crimeDataFilePath = "character",
   crimeDataSP = "SpatialPointsDataFrame"
 ),
 methods = list(
   init = function() {
     crimeDataSP <<- getCrimeData()
     mapDataSP <<- getMap()
   },
   getMap = function() {
     comMap <- readOGR(mapFilePath)
     comMap@data <- comMap@data[ , c(6,16)]
     names(comMap@data) <- c("nazwa", "powierzchnia")
     comMap <- spTransform(comMap, CRS("+init=epsg:4326"))
     
     return (comMap)
   },
   getCrimeData = function() {
     categoryName <- 'KAT'
     latName <- 'LAT'
     lngName <- 'LNG' 
     
     crimeData <- read.csv(file = crimeDataFilePath, sep = "|")
     filteredCrimeData = crimeData[ which(crimeData[[latName]] != '' & crimeData[[lngName]] != ''), ]
     category <- filteredCrimeData[[categoryName]]
     
     lat <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[latName]])))
     lng <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[lngName]])))
     coords <- data.frame(lng, lat)
     names(coords) <- c("x", "y")
     
     data <- data.frame(category)
     
     return (SpatialPointsDataFrame(coords, data, proj4string = CRS("+init=epsg:4326"), match.ID=TRUE))
   },
   drawPlot = function() {
     int <- gIntersects(crimeDataSP, mapDataSP, byid = T)
     
     clipped <- apply(!int, MARGIN = 2, all)
     crime.sp2 <- crimeDataSP[which(!clipped), ]
     
     aggr <- aggregate(x = crime.sp2["category"], by = mapDataSP, FUN = length)
     
     mapDataSP@data$category <<- aggr@data$category
     
     map.gg <- fortify(mapDataSP, region="nazwa")
     map.gg <- merge(map.gg, mapDataSP@data, by.x="id", by.y="nazwa", sort=FALSE)
     
     ggplot() + geom_polygon(data = map.gg, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1)
   }
 )
)