library(dplyr)
library(tidyr)
library(ggplot2)

# data yang digunakan adalah data tracking gps-nya Google
# saya posisikan gps ponsel pada mode on selama tahun 2019
# memuat data (ini sudah dirapikan, aslinya json atau xml, lupa)
# jika ingin sample data yang saya pakai silakan
# unduh di https://akherlan.github.io/databoard dengan nama sample_gmap_data
google_maps_visited_2019 <- 
  readRDS("data/google_maps_visited_2019.rds")

# lihat-lihat dulu seperti apa isinya
glimpse(google_maps_visited_2019)
unique(google_maps_visited_2019$name)
google_maps_visited_2019[google_maps_visited_2019$name=="KFC",]

# ternyata ada dua KFC yang koordinatnya berbeda
# visit https://gps-coordinates.org/distance-between-coordinates.php
# both KFC's distance are ~3km!
# ini tidak bisa dibiarkan!
# bikin id unik untuk masing-masing nama tempat dengan komposisi koordinat

place <- google_maps_visited_2019 %>% 
  select(lon, lat) %>% 
  unite("id", lon:lat, sep = ",") %>% 
  bind_cols(., google_maps_visited_2019)

# seharusnya kita pakai parameter toleransi distance-nya, tapi
# kayaknya bakal panjang, dan juga
# belum cari cara ngitung distance dari koordinat
# sementara boleh pake koordinat unik dulu deh~

# apa ada kejadian dimana satu id unik dinamai berbeda? --------
# thanks to:
# https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r)
place_unique <- unique(place[1:2])
occurance <- data.frame(table(place_unique$id)) %>% `names<-`(c("id", "freq"))
place_unique[place_unique$id %in% occurance$id[occurance$freq>1],]

# jadikan agar satu id cuma punya satu nama
place[place$id=="106.789772,-6.2084127",]$name <- "Ruang & Tempo"
place[place$id=="106.790123,-6.2087326",]$name <- "PT Tempo Inti Media Tbk"

# generate ulang karena sudah diseragamkan namanya
place_unique <- unique(place[1:2])

# tambah kolom frekuensi
freq <- data.frame(table(place$id)) %>% `names<-`(c("id", "freq"))
place_unique <- left_join(place_unique, freq, by = "id")

# tambah kolom total durasi
place_unique <- place %>% 
  group_by(id) %>% 
  summarise(dur_acc_sec = sum(dur)) %>% 
  left_join(place_unique, ., by = "id")

# spread id, jadikan data koordinat lagi seperti semula
place_unique <- separate(place_unique, id, c("lon", "lat"), sep=",")
place_unique$lon <- as.numeric(place_unique$lon)
place_unique$lat <- as.numeric(place_unique$lat)

# data sering (durasi panjang)
sering <- head(arrange(place_unique, desc(dur_acc_sec)), n = 11)
sering$dur_hari <- round(((sering$dur_acc_sec)/(60*60*24)))

# data jarang (durasi singkat)
jarang <- head(arrange(place_unique, dur_acc_sec), n = 10)
jarang$dur_menit <- round((jarang$dur_acc_sec/(60)), 2)

# plot tempat durasi panjang
ggplot(sering) +
  geom_bar(aes(x = name, y = dur_hari), stat = "identity") +
  coord_flip() +
  labs(title = "Tempat yang paling sering Andi ada selama 2019",
       subtitle = "Sungguh tipu daya saat GPS bilang saya ngekos di masjid!",
       caption = "Data gps dari Google Maps (saya selalu on-kan)",
       y = "Durasi (hari)",
       x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 120, 30)) +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# plot tempat durasi singkat 
ggplot(jarang) +
  geom_bar(aes(x = name, y = dur_menit), stat = "identity") +
  coord_flip(ylim = c(4.5,6)) +
  labs(title = "Tempat yang cuma sebentar Andi ada selama 2019",
       subtitle = "Sekali-kali main, lewat: SPBU, tempat makan, halte, apotek",
       caption = "Data gps dari Google Maps (saya selalu on-kan)",
       y = "Durasi (menit)",
       x = element_blank()) +
  scale_y_continuous(breaks = seq(4.5,6.0,0.5)) +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# data tambahan
# unduh peta dari gadm: (https://gadm.org/download_country_v3.html)
map <- readRDS("data/gadm36_IDN_1_sf.rds")

# plot tempat yang pernah disinggahi
ggplot(data = place_unique) +
  geom_sf(data = map, fill = "lightgrey") +
  geom_point(aes(lon, lat), color = "red") +
  coord_sf(
    xlim = c(min(place_unique$lon) - 0.2, max(place_unique$lon) + 0.2),
    ylim = c(min(place_unique$lat) - 0.2, max(place_unique$lat) + 0.2)
  ) +
  labs(
    title = "Tempat yang pernah Andi ada selama 2019",
    subtitle = "Sekitaran Banten - DKI - Jabar - Jateng",
    caption = "Data gps dari Google Maps (saya selalu on-kan)",
    y = "Lintang",
    x = "Bujur"
  ) +
  theme_classic()

# animasi?
library(gganimate)
ggplot(data = google_maps_visited_2019) +
  geom_sf(data = map, fill = "lightgrey") +
  geom_point(aes(lon, lat), color = "red", alpha = 0.6) +
  coord_sf(
    xlim = c(min(place_unique$lon) - 0.2, max(place_unique$lon) + 0.2),
    ylim = c(min(place_unique$lat) - 0.2, max(place_unique$lat) + 0.2)
  ) +
  theme_classic() +
  labs(
    title = "Tempat yang pernah Andi ada selama 2019",
    subtitle = "Tanggal: {frame_time}",
    caption = "Data gps dari Google Maps (saya selalu on-kan)",
    y = "Lintang",
    x = "Bujur"
  ) +
  transition_time(start) +
  ease_aes("linear")