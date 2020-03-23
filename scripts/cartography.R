library(sf)
library(cartography)

# loading data
mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
# set image canvas
png("postlegend1.png", width = 474, height = 555)
par(mar = c(0,0,0,0))
# plot geometric data
plot(st_geometry(mtq))
# set legend position
legpos <- c(717200, 1637100)
points(x = legpos[1], y = legpos[2])
# plot legend
propSymbolsLayer(x = mtq, var = "POP", legend.pos = legpos)
dev.off()

# define breaks values for legend
bks <- getBreaks(v = mtq$MED, nclass = 5, method = "quantile")
# define color
cols <- carto.pal(pal1 = "turquoise.pal", n1 = 5)
# plot a choropleth layer
choroLayer(x = mtq, var = "MED", breaks = bks, col = cols, colNA = "white")
# plot north arrow interactively
north(pos = unlist(locator(1)))
# plot scalebar interactively
barscale(pos = unlist(locator(1)), size = 5)

# source: https://rgeomatic.hypotheses.org/1837 ----