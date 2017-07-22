GeoCrime <- setRefClass("GeoCrime",
 fields = list(
   mapFilePath = "character",
   mapDataSP = "SpatialPolygonsDataFrame",
   crimeDataFilePath = "character",
   crimeData = "data.frame",
   crimeDataSP = "SpatialPointsDataFrame",
   crimeInRegionDataSP = "SpatialPointsDataFrame",
   crimeAggregateData = "data.frame"
 ),
 methods = list(
   init = function() {
     crimeData <<- getCrimeData()
     crimeDataSP <<- getCrimeDataSP()
     mapDataSP <<- getMap()
     crimeInRegionDataSP <<- getCrimeDataWithinBoarders()
     crimeAggregateData <<- getAggrateData()
   },
   getMap = function() {
     comMap <- readOGR(mapFilePath)
     comMap@data <- comMap@data[ , c(6,16)]
     names(comMap@data) <- c("nazwa", "powierzchnia")
     comMap <- spTransform(comMap, CRS("+init=epsg:4326"))
     
     return (comMap)
   },
   getCrimeData = function() {
     return (read.csv(file = crimeDataFilePath, sep = "|"))
   },
   getCrimeDataSP = function() {
     categoryName <- 'KAT'
     latName <- 'LAT'
     lngName <- 'LNG' 
     
     filteredCrimeData = crimeData[ which(crimeData[[latName]] != '' & crimeData[[lngName]] != ''), ]
     category <- filteredCrimeData[[categoryName]]
     
     lat <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[latName]])))
     lng <- as.numeric(gsub(",", ".", gsub("\\.", "", filteredCrimeData[[lngName]])))
     coords <- data.frame(lng, lat)
     names(coords) <- c("x", "y")
     
     data <- data.frame(category)
     
     return (SpatialPointsDataFrame(coords, data, proj4string = CRS("+init=epsg:4326"), match.ID=TRUE))
   },
   getCrimeDataWithinBoarders = function() {
     int <- gIntersects(crimeDataSP, mapDataSP, byid = T)
     
     clipped <- apply(!int, MARGIN = 2, all)
     return (crimeDataSP[which(!clipped), ])
   },
   getCrimeCategoryColour = function() {
     dict <- data.frame(
       id = c("ALK", "BEZP", "CHU", "GOSP", "KRA", "LEG", "OŚ", "PORZ", "RD", "ZWIE"),
       value = c("green", "hotpink", "indianred", "lightsalmon", "mediumorchid", "orange", "paleturquoise", "yellow", "wheat", "thistle"))
     
     return (dict[crimeInRegionDataSP$category, 2, drop=F]$value)
   },
   getAggrateData = function() {
     aggr <- aggregate(x = crimeInRegionDataSP["category"], by = mapDataSP, FUN = length)
     mapDataSP@data$category <<- aggr@data$category
     
     map.gg <- fortify(mapDataSP, region="nazwa")
     return (merge(map.gg, mapDataSP@data, by.x="id", by.y="nazwa", sort=FALSE))
   },
   drawPlot = function() {
     return (ggplot() + geom_polygon(data = crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1))
   }
 )
)