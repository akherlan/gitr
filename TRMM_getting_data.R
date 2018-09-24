# Define a temporal extent
twindow <- seq(as.Date("2016-01-01"), as.Date("2016-03-31"), by = "months")

# Retrieve mean monthly precipitations based on a bounding box and time extent
TRMMfile <- TRMM(twindow = twindow, areaBox = areaBox)
# tapi perintah TRMM gak jalan

plot(TRMMfile)