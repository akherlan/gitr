# Filter GRDC catalogue based on a country code
GRDC_IT <- catalogueGRDC(columnName = "country_code", columnValue = "IT")

# Convert the table to a SpatialPointsDataFrame
hydro <- SpatialPointsDataFrame(coords = GRDC_IT[, c("long", "lat")], data = GRDC_IT)

# Plot the stations on the map
plot(Italy, col = NA, border = "darkgrey")
plot(hydro, add = TRUE, col = "blue", pch = 1)