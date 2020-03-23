library(jsonlite)

# bikin loop untuk ini <----

mon <- fromJSON("~/Desktop/takeout/location_history/semantic_location_history/2019/2019_DECEMBER.json")

name <- mon$timelineObjects$placeVisit$location$name
lat <- mon$timelineObjects$placeVisit$location$latitudeE7
long <- mon$timelineObjects$placeVisit$location$longitudeE7
dur <- mon$timelineObjects$placeVisit$duration

des <- data.frame(name, long, lat, dur)

# ---->

visited <- bind_rows(jan, feb, mar, apr, mei, jun, jul, ags, sep, okt, nov, des)

visit <- visit[complete.cases(visit),]
visit[,4] <- as.numeric(visit[,4])
visited <- mutate(visit, dur = visit[,5] - visit[,4])
as_tibble(visited)

library(lubridate)

visited <- visited %>% mutate(dur = as.duration(
as_datetime(visited[,5]*10^-3, tz = "Asia/Jakarta") -
  as_datetime(visited[,4]*10^-3, tz = "Asia/Jakarta")))
visited <- as_tibble(visited[,-6])
names(visited) <- c("name", "lon", "lat", "start", "end", "dur")

visited[,4] <- as_datetime(visited[,4]*10^-3, tz = "Asia/Jakarta")
visited[,5] <- as_datetime(visited[,5]*10^-3, tz = "Asia/Jakarta")

visited$lon <- visited$lon*10^-7
visited$lat <- visited$lat*10^-7

library(sf)
library(rnaturalearth)
idn <- ne_countries(scale = "medium", country = "indonesia", returnclass = "sf")

library(ggplot2)
ggplot(data = visited) +
  geom_sf(data = idn) +
  geom_point(aes(lon, lat, size = log10(as.numeric(dur))), alpha = 0.5, show.legend = F) +
  coord_sf(
    xlim = c(min(visited$lon)-1,max(visited$lon)+1), 
    ylim = c(min(visited$lat)-1,max(visited$lat+1))
  ) +
  theme_void()

library(mapview)
mapview(visited$lon, visited$lat)
