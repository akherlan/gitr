# unduh data TABLE 53
# url: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Sep%202017?OpenDocument
# sumber: https://www.r-bloggers.com/an-r-tutorial-to-download-and-plot-some-queensland-population-data/

workdir <- "~/Desktop" # ganti ke lokasi data
setwd(workdir)

library('readxl')
qld <- read_excel(
  path = '3101053.xls', 
  sheet = 'Data1', 
  range = 'A10:GU57'
)

columns <- read_excel(
  path = '3101053.xls', 
  sheet = 1, 
  range='A13:A214', 
  col_types = 'text', 
  col_names = 'Header'
)

library('stringr')
columns$Header <- columns$Header %>% 
  str_remove(pattern = "Estimated Resident Population ;  ") %>% 
  str_remove(pattern = "\ ; ") %>% 
  str_remove(pattern = "and over") %>% 
  str_remove(pattern = ";") %>% 
  str_trim(side = 'right')

colnames(qld) <- c('Year', columns$Header)

library(tidyr)
qld_pop <- qld %>%
  gather(Sex_Age, Population, -'Year') %>% 
  separate(Sex_Age, into = c('Sex', 'Age'), sep = " ")

qld_pop$Year <- year(qld_pop$Year)

qld_pop$Age <- as.integer(qld_pop$Age)

# plot

qld_pop %>% filter(Year == 2017) %>%
  ggplot(aes(x=Age, y=Population, color=Sex)) +
  geom_line(size=1.1, alpha=0.8) +
  labs(x = "Age",
       y = 'Number of people',
       title = "Population of Queensland in 2017 by sex and age.") +
  theme(
    rect = element_rect(
      fill = 'white', linetype = 0, colour = NA, size=NA), 
    panel.background = element_blank(), 
    panel.grid.minor.x = element_line(colour = NA, size = NA), 
    panel.grid.minor.y = element_line(colour = 'gray90', size = 0.2),
    panel.grid.major.x = element_line(size = NA,  colour = NA),
    panel.grid.major.y = element_line(
      size = 0.3, 
      linetype = 'solid', 
      colour = "gray80"),
    strip.background = element_rect(fill = "grey80", colour = "grey20"),
    legend.key = element_rect(fill="transparent", colour=NA)
  )
