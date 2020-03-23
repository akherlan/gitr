# source
# https://austinschaub.com/2016/11/02/r-tutorial-scrape-html-table-using-hadleys-rvest-package/
# https://stackoverflow.com/questions/43413322/how-to-convert-rows-into-columns-using-dplyr

# load libraries
library("rvest")
library("dplyr")

# get url
url <- "http://hubud.dephub.go.id/?id/bandara/index/"

# parsing webpage
page <- read_html(url)

# get the table in webpage
airportlist <- html_table(page, fill = TRUE)
glimpse(airportlist)

# select true table
airport_table <- airportlist[[4]]

# select true column
airport_table <- airport_table[,1:3]
airport_table[[1,3]] <- airport_table[[1,2]]
airport_table <- airport_table[,c(-2)]
names(airport_table) <- c("head", "value")
airport_table[c(1,8,15,22,29,36,43,50,57,64),1] <- "Nama"
airport_table <- airport_table[c(-2,-9,-16,-23,-30,-37,-44,-51,-58,-65),]

# reshape the table
airport_table <- unstack(x = airport_table, form = value ~ head)
str(airport_table)
airport_table <- select(airport_table, Nama, IATA...ICAO, Kategori, Kelas, Pengelola, Alamat)
