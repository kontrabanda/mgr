drawAreaDensityPlot <- function(map, sp, title, label)
{
  aggr <- aggregate(x = sp["category"], by = map, FUN = length)
  
  map@data$category <- aggr@data$category
  
  EPSG <- make_EPSG()
  EPSG[grepl("WGS 84$", EPSG$note), ]
  
  map.gg <- fortify(map, region="nazwa")
  map.gg <- merge(map.gg, map@data, by.x="id", by.y="nazwa", sort=FALSE)
  
  map <- ggplot() + geom_polygon(data = map.gg, aes(long, lat, group = group, fill = category), colour = "black", lwd=0.1) + ggtitle(title) + labs(x = "E", y = "N", fill = label)
  map + scale_fill_gradient(low = "white", high = "red")
}