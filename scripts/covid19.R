## Melihat Perkembangan Korona Di Indonesia ------------------------
# data tidyed by Mar 24th, 2020 on 11.00 WIB (UTC + 7)
# library yang dipakai
if(!require("dplyr")) install.packages("dplyr")
if(!require("readr")) install.packages("readr")
if(!require("purrr")) install.packages("purrr")
if(!require("pins")) install.packages("pins")

# terima kasih untuk teman-teman DSCI
# semoga kebaikan kalian dibalas berlipat oleh Allah S.W.T
# data tersedia di kaggle.com/ardisragen ---------------------------
# buat token di kaggle lalu:
# pins::board_register("kaggle", token = "~/.kaggle/kaggle.json")
data <- pins::pin_get("ardisragen/indonesia-coronavirus-cases", "kaggle")

# karena error waktu pakai utils::read.csv() pada file province.csv
# jadi saya pakai readr::read_csv()
cases <- read_csv(data[1])
confirmed <- read_csv(data[2])
trend <- read_csv(data[3])
patient <- read_csv(data[4])
province <- read_csv(data[5]) 

## rapikan data cases dan confirmed --------------------------------
# ubah date ke tipe data date
cases$date <- as.Date(cases$date, format = "%d-%B-%y")
confirmed$date <- as.Date(confirmed$date, format = "%m/%d/%Y")

# data confirmed sudah include dalam cases di kolom acc_confirmed
tail(cases); tail(confirmed)

# buang satuan %, jadikan data numeric
txt_to_num_percent <- function(col){
  col <- as.numeric(gsub("%","", col))
  return(col)
}

cases[c("positive_rate", "negative_rate", 
        "decease_rate", "release_rate")] <-
  lapply(
    cases[c("positive_rate", "negative_rate", 
            "decease_rate", "release_rate")], 
    txt_to_num_percent)

# data hari terakhir cases kosong, buang
nrow(cases)
cases <- (cases[-22,])


## rapikan data trend ----------------------------------------------
# ubah nama Day jadi date supaya match dengan lainnya
colnames(trend)[1] <- "date"
trend$date <- as.Date(trend$date, format = "%m/%d/%Y")


## rapikan data patient --------------------------------------------
glimpse(patient)
patient %>% select_if(is.character) %>% purrr::map(~unique(.))

# ubah ke data factor
patient[c("gender", "nationality", "province", 
          "current_state", "hospital")] <-
  lapply(
    patient[c("gender", "nationality", "province", 
            "current_state", "hospital")],
    as.factor)

# ubah ke data date
patient[c("confirmed_date", "released_date", "deceased_date")] <-
  lapply(patient[c("confirmed_date", "released_date", "deceased_date")],
         as.Date, format = "%d-%B-%y")


## rapikan data province -------------------------------------------
province$province_name <- gsub("^\\\xa0", "", province$province_name)

# save data --------------------------------------------------------
d <- "output/covid19/"
dir.create(d)
saveRDS(cases, paste0(d,"cases.rds"))
saveRDS(confirmed, paste0(d,"confirmed_acc.rds"))
saveRDS(patient, paste0(d,"patient.rds"))
saveRDS(province, paste0(d,"province.rds"))
saveRDS(trend, paste0(d,"keywords_trend.rds"))