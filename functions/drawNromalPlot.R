drawNromalPlot <- function(geoCrime) {
  ggplot() + 
    geom_polygon(data = geoCrime$crimeAggregateData, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) +
    geom_point(data = data.frame(geoCrime$crimeInRegionDataSP), aes(x, y), colour = "green", size=2, alpha=1/4)
}