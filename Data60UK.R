UK <- getData("GADM", country = "GBR", level = 0)
plot(UK, col = NA, border = "darkgrey")

# Data60UK full catalogue
allData60UK <- catalogueData60UK()
hydroALL <- SpatialPointsDataFrame(coords = allData60UK[, c("Longitude", "Latitude")], data = allData60UK)

plot(hydroALL, add = TRUE, col = "blue", pch = 1)

# Extract time series for the first station
stationID <- catalogueData60UK()$stationID[1]

# Extract time series for a specified temporal window
twindow <- seq(as.Date("1988-01-01"), as.Date("1989-12-31"), by = "days")
MorwickTSplot <- tsData60UK(stationID = stationID,
                            plotOption = TRUE,
                            twindow = twindow)