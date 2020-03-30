library('readr') # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
library("purrr")# data manipulation
library("stringr")#String manipulation
library("gridExtra")
library('viridis')
options(repr.plot.width=4, repr.plot.height=3)
options(warn = -1)

flavors_of_cacao <- read.csv('../input/flavors_of_cacao.csv', stringsAsFactors = FALSE)

head(flavors_of_cacao)

summary(flavors_of_cacao)
#We have 1795 observations and 9 variables

names(flavors_of_cacao)[1:9] <- c("Company", "Sp_origin", "Reference", "Review_date", "Cocoa_percent", "Company_location", "Rating", "Bean_type", "Broad_origin")

flavors_of_cacao$Cocoa_percent <- as.numeric(gsub("[%]", "", flavors_of_cacao$Cocoa_percent))

flavors_of_cacao[, c(8,9)] <- sapply(flavors_of_cacao[,c(8,9)], str_trim)
  is.na(flavors_of_cacao) <- flavors_of_cacao==''

#Checking the percent of NAs
colMeans(is.na(flavors_of_cacao))
head(flavors_of_cacao)

options(repr.plot.width=6, repr.plot.height=4)

top_companies <- flavors_of_cacao %>% group_by(Company) %>% summarise(Count= n())%>%
  top_n(10, wt = Count)%>%arrange(desc(Count))

ggplot(top_companies, aes(reorder(Company, Count),  Count, fill = Count)) + coord_flip()+
  geom_bar(stat = "identity", fill = 'chocolate4', aes(color = I('black')), size = 0.1)+xlab('Top_Companies')+
  theme_few()

flavors_of_cacao %>% group_by(Review_date) %>% summarise(Count= n())%>%
  ggplot(aes(x =factor(Review_date), y = Count, fill = Count)) + 
    geom_bar(stat = "identity", fill = 'chocolate4', aes(color = I('black')), size = 0.1) +
  xlab("Review Date") +
  theme_few()

ggplot(flavors_of_cacao ,aes(x =Cocoa_percent, fill = Cocoa_percent)) + geom_histogram(fill = 'chocolate4', bins = 60) + 
  scale_fill_gradient(trans = 'reverse')+scale_x_continuous(breaks = seq(40, 100, by = 5)) +
  theme_few()

flavors_of_cacao %>% group_by(Company_location) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>% top_n(10, wt = pct)%>%
    ggplot(aes(x =reorder(Company_location,pct), y =pct, fill = pct)) + geom_bar(stat = "identity", fill = "chocolate4", aes(color = I('black')), size = 0.1) + 
    coord_flip()+xlab("Countries") +ylab("Percentage")+
    theme_few()

ggplot(flavors_of_cacao, aes(factor(Rating))) + geom_bar(fill = "chocolate4", aes(color = I('black')), size = 0.1)+xlab("Rating") +
    theme_few()

flavors_of_cacao <- mutate(flavors_of_cacao, Rating_grp = Rating)
  
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >=1 & flavors_of_cacao$Rating_grp < 2] <- "Unpleasant"
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >=2 & flavors_of_cacao$Rating_grp < 3] <- "Disappointing"
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >=3 & flavors_of_cacao$Rating_grp <= 3.75] <- "Satisfactory to praiseworthy"
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp == 4] <- "Premium"
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp == 5] <- "Elite"
 flavors_of_cacao$Rating_grp <- factor(flavors_of_cacao$Rating_grp, levels = c("Unpleasant", "Disappointing", "Satisfactory to praiseworthy", 
                                                                               "Premium", "Elite"))
 flavors_of_cacao %>% group_by(Rating_grp) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>%
   ggplot(aes(x =reorder(Rating_grp,pct), y =pct, fill = pct)) + geom_bar(stat = "identity", fill = 'chocolate4', aes(color = I('black')), size = 0.1)+ 
   xlab("Ratings Groups") +ylab("Percentage")+coord_flip()+
   theme_few()

options(repr.plot.width=8, repr.plot.height=6)
flavors_of_cacao <- mutate(flavors_of_cacao, Cocoa.pct = Cocoa_percent)
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=40 & flavors_of_cacao$Cocoa.pct < 50] <- '40-49pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=50 & flavors_of_cacao$Cocoa.pct < 60] <- '50-59pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=60 & flavors_of_cacao$Cocoa.pct < 70] <- '60-69pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=70 & flavors_of_cacao$Cocoa.pct < 80] <- '70-79pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=80 & flavors_of_cacao$Cocoa.pct < 90] <- '80-89pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=90 & flavors_of_cacao$Cocoa.pct <= 99] <- '90-99pct'
flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct== 100] <- '100pct'

 flavors_of_cacao$Cocoa.pct <- factor(flavors_of_cacao$Cocoa.pct, levels = c("40-49pct", "50-59pct", "60-69pct", 
                                                                               "70-79pct", "80-89pct", "90-99pct","100pct"))

ggplot(flavors_of_cacao ,aes(x =reorder(Rating_grp, Rating_grp), fill =Cocoa.pct )) + 
geom_bar(position = "dodge", aes(color = I("black")), size = 0.1) + xlab("Rating") + scale_fill_brewer(palette="Oranges")+
theme_few()+ coord_flip()

options(repr.plot.width=10, repr.plot.height=6)
ggplot(data = flavors_of_cacao) +
  theme_bw() +
  geom_tile(aes(x =Company_location, y = Rating , fill = Cocoa.pct)) +
  scale_fill_brewer(palette="Oranges") + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), 
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Country") + ylab("Rating")+ ggtitle("Ratings w.r.t Cocoa percent")

flavors_of_cacao %>% group_by(Bean_type) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>% top_n(10, wt = pct)%>%
   ggplot(aes(x =reorder(Bean_type,pct), y =pct, fill = pct)) + geom_bar(stat = "identity", fill = "chocolate4", aes(color = I('black')), size = 0.1) + coord_flip()+
    xlab("Bean_Type") +ylab("Percentage")+
   theme_few()

options(repr.plot.width=8, repr.plot.height=6)
flavors_of_cacao %>% na.omit %>% filter(Rating_grp == 'Elite' | Rating_grp == 'Premium') %>% 
group_by(Broad_origin, Rating_grp) %>% summarise(Count = n())%>% arrange(desc(Count)) %>% head(10)%>% 
ggplot(aes(x =reorder(Broad_origin,Count), y =Count)) + 
geom_bar(stat = "identity", fill = 'chocolate4', aes(color = I('black')), size = 0.1) +
coord_flip()+ xlab("Countries") +ylab("Number of Bars reviewed")+
  ggtitle("Where are the best Cocoa beans grown")+
theme(plot.title = element_text(hjust = 0.5))

options(repr.plot.width=6, repr.plot.height=4)
corrplot(cor(flavors_of_cacao[,unlist(lapply(flavors_of_cacao, is.numeric))], use = "complete.obs"), type = "lower")

options(repr.plot.width=12, repr.plot.height=6)
flavors_of_cacao$Rating_grp = factor(flavors_of_cacao$Rating_grp,
                                     levels=c("Unpleasant", "Disappointing", "Satisfactory to praiseworthy", 
                                                                               "Premium", "Elite"),ordered=TRUE)
flavors_of_cacao %>% group_by(Company_location, Rating_grp) %>% summarise(Count = length(Rating_grp))%>%
ggplot(aes(x =Company_location, y =Count, fill = Rating_grp)) +  
geom_bar(position = 'fill',stat = "identity", aes(color = I('black')), size = 0.1) +scale_fill_brewer(palette="Oranges")+ xlab("Countries") +
ylab("Number of Bars reviewed")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
