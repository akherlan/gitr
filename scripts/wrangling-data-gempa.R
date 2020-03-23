# muat data dari csv
files <- fs::dir_ls("~/Downloads/datagempa/", regexp = "[0-9]{4}\\.csv")
gempa <- purrr::map_dfr(files, read.csv)

# data wrangling
exclude <- gempa[1,3]
gempa <- gempa[gempa$WAKTU != exclude,]
n <- names(quake)
gempa <- gempa[,2:14]
names(gempa) <- n

# tidy data tanggal
gempa$tanggal <- as.Date(gempa$tanggal, format = "%m/%d/%Y")

# tidy data koordinat
gempa$lat <- gsub("^(\\d+)\\.?(\\d+)? LU$", "\\1\\.\\2", gempa$lat)
gempa$lat <- gsub("^(\\d+)\\.?(\\d+)? LS$", "-\\1\\.\\2", gempa$lat)

gempa$lon <- gsub("^(\\d+)\\.?(\\d+)? BT$", "\\1\\.\\2", gempa$lon)
gempa$lon <- gsub("^(\\d+)\\.?(\\d+)? BB$", "-\\1\\.\\2", gempa$lon)

gempa$lat <- as.double(gempa$lat)
gempa$lon <- as.double(gempa$lon)

# tidy data kedalaman
gempa$kdlm.KM <- as.numeric(gsub("^(\\d+)\\.?(\\d+)? Km$", "\\1\\.\\2", gempa$kdlm.KM))

# tidy data magnitudo
gempa$mag.SR <- as.numeric(gsub("^(\\d+)\\.?(\\d+)? SR$", "\\1\\.\\2", gempa$mag.SR))

# tidy data tipe magnitudo
gempa[gempa$typemag == "Mw(mB",]$typemag <- "Mw(mB)"
gempa$typemag <- as.factor(gempa$typemag)

gempa <- dplyr::as_tibble(gempa)

plot(gempa$tanggal, gempa$mag.SR)