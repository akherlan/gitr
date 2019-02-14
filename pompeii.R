# Plot Pompeii on a map
library(raster)
Italy <- getData("GADM", country = "IT", level = 0)
Pompeii <- SpatialPoints(coords = data.frame(x = 14.4870, y = 40.7510))
plot(Italy, col = NA, border = "darkgrey")
plot(Pompeii, add = TRUE, col = "red")

# Define and plot a bounding box centred in Pompeii (Italy)
areaBox <- raster::extent(Pompeii@coords[[1]] - 0.5, Pompeii@coords[[1]] + 0.5, Pompeii@coords[[2]] - 0.5, Pompeii@coords[[2]] + 0.5)
plot(areaBox, add = TRUE, col = "red")
