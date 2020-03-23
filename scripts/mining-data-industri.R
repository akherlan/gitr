library(osmdata)
library(dplyr)

# bikin query
q <- getbb("karawang") %>% # ambil batas karawang
  opq(timeout = 250) %>% # set slot query
  add_osm_feature(key = "landuse", value = "industrial") # tambah filter

industry <- osmdata_sf(q) # tarik data pakai query q
print(industry) # cuma ada dua yang terisi data: point, polygon

as_tibble(industry$osm_points) # harus pakai reverse geocoding
as_tibble(industry$osm_polygons) # mungkin bisa dipakai
industry <- industry$osm_polygons # ambil data polygon saja
mapview::mapview(industry) # lihat data
