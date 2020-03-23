# Analisis Grup Whatsapp -----

library(tidyverse)
library(lubridate)

# muat data

wa_raw <-
  read_delim(
    file = "~/Downloads/Telegram Desktop/WhatsApp Chat with SMTS07A.txt", 
    delim = "-", 
    col_names = c("date", "post")
  )

print(wa_raw)

# bersihkan

wa <- wa_raw %>% 
  setNames(c("date", "post")) %>% 
  mutate(post = str_trim(post)) %>% 
  filter(post %>% str_detect(":")) %>% 
  separate(post, into = c("person", "post"), sep = ":") %>% 
  mutate(
    date = date %>% parse_date_time("%m/%d/%y, %I:%M %p"),
    hour = hour(date),
    year = year(date),
    wday = wday(date, label = TRUE, abbr = TRUE)
  ) %>% 
  filter(year == 2019)

print(wa)

nrow(wa_raw) - nrow(wa)

# hitung chat per jam

hour_count <- wa %>% 
  count(hour) %>% 
  drop_na(hour)

first_hour <- # tambahan biar 0/24
  hour_count %>%  
  filter(hour == 0) %>% 
  mutate(hour = 24)

hour_count_extended <- # setelah bersatu
  hour_count %>%  
  rbind(first_hour)

# formula koordinat radar bikinan -----

coord_radar <-
  function(theta = "x", start = 0, direction = 1){
    theta <- match.arg(theta, c("x","y"))
    r <- if (theta == "x") "y" else "x"
    ggproto(
      "CordRadar", CordPolar,
      theta = theta,
      r = r,
      start = start,
      direction = sign(direction),
      is_linear = function(coord) TRUE
    )
  }

# formula koordinat radar beneran -----

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# plot pertama -----

ggplot(hour_count_extended, aes(x = hour, n)) +
  geom_polygon(
    fill = "#009688",
    group = 1,
    color = "#4cb5ab",
    alpha = .7
  ) +
  geom_point(
    color = "#99d5cf",
    size = 1.3,
    alpha = .8
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 1)
  ) +
  coord_radar() +
  theme_minimal()

# plot gelap -----

ggplot(hour_count_extended, aes(x = hour, n)) +
  geom_polygon(
    fill = "#009688",
    group = 1,
    color = "#4cb5ab",
    alpha = .7
  ) +
  geom_point(
    color = "#99d5cf",
    size = 1.3,
    alpha = .8
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 1)
  ) +
  coord_radar() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#22292F"),
    plot.margin = unit(rep(2, 4), "cm"),
    axis.text.x = element_text(size = 15, colour = "#c6ced6"),
    axis.title = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(colour = "#364049", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#1e242a", color = "#22292F"),
    panel.spacing = unit(3, "lines")
  )

# plot dengan text -----

library(ggtext) # package still not available for R 3.6.1

# siapa teman paling aktif? -----

count_person <- wa %>% 
  count(group_by = person, sort = TRUE) %>% 
  setNames(c("person", "chat"))

count_person

ggplot(data = count_person, aes(person, chat)) +
  geom_bar(
    stat = "identity", 
    width = 0.4, 
    colour = "blue", 
    fill = "lightblue"
  ) +
  coord_flip() +
  theme_minimal()

ggplot(data = count_person, aes(reorder(person, chat), chat)) +
  geom_col(width = 0.4, colour = "dodgerblue", fill = "lightblue") +
  coord_flip() +
  geom_text(
    aes(label = chat), 
    position = position_dodge(width = 0.9),
    hjust = -0.5,
    size = 2.5,
    colour = "darkblue"
  ) +
  labs(
    title = "Intensitas kirim pesan",
    subtitle = "1 Jan - 3 Des 2019",
    caption = "Sumber data: WhatsApp Group SMTS07A",
    x = "",
    y = ""
  ) +
  theme_minimal()

# time series percakapan grup -----

data <- wa %>%
  count(group_by = date(date)) %>%
  setNames(c("date", "n"))

plot <- 
ggplot() +
  geom_line(
    data = data,
    aes(date, n), 
    color = "darkgrey", 
    alpha = .6
  ) +
  labs(
    x = "Waktu",
    y = "Jml pesan",
    title = "Tingkat keramaian Grup WA SMTS07A",
    subtitle = "1 Jan - 3 Des 2019"
  ) +
  scale_x_date(
    date_labels = "%b", 
    date_breaks = "1 month"
  ) +
  theme_minimal()

plot

chat_hendi <- wa %>% 
  filter(person == "Hendi Kurniawan") %>% 
  count(group_by = date(date)) %>% 
  setNames(c("date", "n"))

chat_andi <- wa %>% 
  filter(person == "Andi K. Herlan") %>% 
  count(group_by = date(date)) %>% 
  setNames(c("date", "n"))

chat_fiqih <- wa %>% 
  filter(person == "Fiqih") %>% 
  count(group_by = date(date)) %>% 
  setNames(c("date", "n"))

plot +
  geom_point(data = data[85,], aes(date, n), color = "red") +
  geom_line(data = chat_hendi, aes(date, n), alpha = .6, color = "red") +
  geom_line(data = chat_andi, aes(date, n), alpha = .6, color = "blue") +
  geom_line(data = chat_fiqih, aes(date, n), alpha = .6, color = "green")


# kapan teman jadi aktif? -----
# dapatkan chat 6 orang teratas

wa_friends <- wa %>% 
  mutate(
    person = person %>% 
      fct_lump(n = 6) %>% 
      fct_infreq
  ) %>% 
  filter(person != "Other")

# hitung chat per orang per jam

hour_count_person <- wa_friends %>%
  count(hour, person) %>% 
  drop_na(person)

# plot

ggplot(data = hour_count_person, aes(x = hour, n)) +
  geom_polygon(
    fill = "#009688",
    group = 1,
    color = "#4cb5ab",
    alpha = .7
  ) +
  geom_point(
    color = "#99d5cf",
    size = 1.3,
    alpha = .8
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  coord_radar() +
  theme_minimal() +
  facet_wrap(~person, ncol = 3)

## siapa yang posting gambar paling banyak? -----

img <- " <Media omitted>"

wa %>% 
  filter(post == img) %>% 
  count(group_by = person, sort = TRUE) %>% 
  setNames(c("person", "shared_images"))

wa %>%
  mutate(person = person %>% as.factor())

# Source: ------
# [link 1](https://ggplot2tutor.com/radar-chart/radar-chart-whatsapp/)
# [link 2](https://www.statworx.com/de/blog/customizing-time-and-date-scales-in-ggplot2/)
# [link 3](https://www.r-graph-gallery.com/218-basic-barplots-with-ggplot2)