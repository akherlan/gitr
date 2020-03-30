library(jsonlite)
library(purrr)
library(listviewer)
library(tibble)
library(dplyr)

# Impot `json`

foods_raw <- fromJSON("data/foodMarkets/retail_food_markets.json",
                      simplifyVector = FALSE)

# Discovery: `data`

str(foods_raw, max.level = 1)
str(foods_raw$data, max.level = 1, list.len = 5)

foods <- foods_raw[["data"]]

# Discovery: `meta` dan `view`

jsonedit(foods_raw[[c("meta", "view")]])
jsonedit(foods_raw[[c("meta", "view", "columns")]])
namkol <- foods_raw[[c("meta", "view", "columns")]] %>% map_chr("name")

# Memberi nama
foods <- map(foods, set_names, namkol)

foods %>% map_chr("DBA Name") %>% head()

(proses <- namkol[namkol != "Location"])

foods[1:3] %>% map_df(`[`, proses)

# safe my life

safe_extract <- function(l, wut) {
  res <- l[wut]
  null_here <- map_lgl(res, is.null)
  res[null_here] <- NA
  res
}
# safe_extract(foods[[67]][14:16])

(markets <- map_df(foods, safe_extract, proses))

# selamatkan location

loc_raw <- map(foods, "Location")
loc_raw[[345]]
i <- which(namkol == "Location")
loc_meta <- foods_raw[[c("meta", "view", "columns")]][[i]]
loc_names <- loc_meta[["subColumnTypes"]] %>% flatten_chr()
loc_raw <- map(loc_raw, set_names, loc_names)
loc_raw[[345]]

ha <- loc_raw %>% 
  map("human_address") %>%
  map_df(fromJSON) %>% 
  set_names(paste0("ha_", names(.)))
head(ha)

ee <- loc_raw %>% 
  map_df(safe_extract, loc_names[loc_names != "human_address"]) %>% 
  mutate_at(vars(latitude, longitude), as.numeric)
head(ee)
ee %>% 
  select(latitude, longitude) %>% 
  map(summary)
(markets <- bind_cols(markets, ha, ee))

markets <- markets %>% 
  mutate_if(is.character, trimws)

nm <- names(markets)
nm <- nm %>% tolower() %>% gsub(" ", "_", .)

markets <- set_names(markets, nm)

markets %>% select(dba_name, city, latitude, longitude, square_footage)

saveRDS(markets, "data/food_markets.rds")
