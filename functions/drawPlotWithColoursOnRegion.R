drawPlotWithColoursOnRegion <- function(geoCrime) {
  dict <- data.frame(
    id = c(5614, 7331, 3718, 60, 1547, 6599, 5228, 1557, 3683, 1952, 20075, 2416, 3519, 612, 17772, 1627, 1472, 416, 2043, 460, 523, 1154, 1234, 14, 24, 225),
    value = c("green", "hotpink", "indianred", "lightsalmon", "mediumorchid", "orange", "paleturquoise", "yellow", "wheat", "thistle", 
              "lightseagreen", "lightslateblue", "limegreen", "magenta4", "navyblue", "orange3", "red3", "seashell3", "plum1", "peru", 
              "yellow3", "steelblue4", "springgreen3", "skyblue4", "seagreen3", "palevioletred3"))
  
  colours <- vector(mode="character", length=length(geoCrime$crimeInRegionDataSP))
  
  for(i in 1:26) {
    log <- geoCrime$crimeInRegionDataSP@data$regions == dict[i, 1]
    colours[log] <- toString(dict[i, "value"])
  }
  
  data <- data.frame(geoCrime$crimeInRegionDataSP)
  data$colours <- colours
  
  ggplot() + 
    geom_polygon(data = geoCrime$crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) +
    geom_point(data = data, aes(x, y), colour = colours, size=2, alpha=1/4)
}