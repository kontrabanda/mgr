drawPlotWithColoursOnCategory <- function(geoCrime) {
  dict <- data.frame(
    id = c("ALK", "BEZP", "CHU", "GOSP", "KRA", "LEG", "OŚ", "PORZ", "RD", "ZWIE"),
    value = c("green", "hotpink", "indianred", "lightsalmon", "mediumorchid", "orange", "paleturquoise", "yellow", "wheat", "thistle"))
  
  
  colours <- dict[geoCrime$crimeInRegionDataSP$category, 2, drop=F]$value
  data <- data.frame(geoCrime$crimeInRegionDataSP)
  
  data$colours <- colours
  
  ggplot() + 
    geom_polygon(data = geoCrime$crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) +
    geom_point(data = data, aes(x, y), colour = colours, size=2, alpha=1/4)
}