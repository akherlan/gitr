library("pins")
library("dplyr")
library("purrr")

# find and get data
# board_register("kaggle", token = "~/.kaggle/kaggle.json")
pin_find("chocolate", "kaggle")
choco <- pin_get("spectoro/chocolate", "kaggle") %>% 
  read.csv(stringsAsFactors = FALSE) %>%
  as_tibble()

# look at data
print(choco)
summary(choco)

# rename column
names(choco) <- c("company", "bar.origin", "ref", "review", 
                  "percent.cocoa", "comp.loc", "rating", 
                  "bean.type", "bean.origin")

# mapping data varieties
choco %>% 
  select_if(is.character) %>% 
  map(~nrow(as.data.frame(unique(.))))

# percent.cocoa = num
unique(choco$percent.cocoa)
choco$percent.cocoa <-
  gsub(pattern = "^(\\d+)\\.?.?%$", 
     replacement = "\\1", 
     choco$percent.cocoa) %>% 
  as.numeric()

# percent.comp.loc = fct
unique(choco$comp.loc)
choco$comp.loc <- as.factor(choco$comp.loc)

# bean.type = chr
unique(choco$bean.type)
choco$bean.type <- stringr::str_squish(choco$bean.type)
choco[choco$bean.type=="",]$bean.type <- NA_character_

# bean.origin = list, fct !!suliiit
unique(choco$bean.origin)
choco$bean.origin <- stringr::str_squish(choco$bean.origin)
# typo "Domincan Republic" = "Dominican Republic"
choco[choco$bean.origin == "Domincan Republic",] # ternyata banyak
choco[choco$bean.origin == "Domincan Republic",]$bean.origin <- "Dominican Republic"

# visualization
library(ggplot2)
theme_set(theme_bw())

ggplot(choco) +
  geom_point(aes(bean.type, percent.cocoa)) + 
  labs(x = "Jenis Biji", y = "%") +
  scale_y_continuous(breaks = seq(40,100,10)) +
  coord_flip()
