# required packages
install.packages("gdistance")
install.packages("raster")
install.packages("sp")
install.packages("igraph")

# load library
library("gdistance")

# 1. Read spatial grid data ----
# using `raster`


# 2. Construct an object of the class TransitionLayer or TransitionStack. Function: transition.

# 3. Correct diagonal connections and projection distortions. Function: geoCorrection.

# 4. Get coordinates for the starting and end points of routes.

# 5. Calculate distances and routes. Functions: accCost, costDistance, commuteDistance, rSPDistance, shortestPath, passage, pathInc.

