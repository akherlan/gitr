# library yang diperlukan
library(sf)
library(dplyr)
library(ggplot2)

# memuat data peta
berkas <- "~/Jobs/PU/SHP/ADM/gadm36_IDN_shp/gadm36_IDN_2.shp"
banten <- st_read(berkas)
banten <- banten %>% 
  filter(NAME_1 == "Banten") %>% 
  select(NAME_2, TYPE_2)

# lihat posisi yang sesuai centroid
banten <- cbind(banten, st_coordinates(st_centroid(banten)))
theme_set(theme_bw())
ggplot(data = banten) + 
  geom_sf(col = "lightgrey") +
  geom_text(aes(X, Y, label = NAME_2), size = 3) +
  coord_sf()

# sediakan "kanvas"
plot(st_geometry(banten))

# posisi label, klik sesuai urutan:
# 1. Cilegon         2. Kota Serang
# 3. Kota Tangerang  4. Lebak
# 5. Pandeglang      6. Serang
# 7. Tangerang       8. Tangsel 
posisi <- locator(8) %>% as.data.frame()

# ganti data posisi label
banten[,4:5] <- posisi

# hasil akhir
ggplot(data = banten) + 
  geom_sf(col = "lightgrey") +
  geom_text(aes(X, Y, label = NAME_2), size = 3) +
  coord_sf()
