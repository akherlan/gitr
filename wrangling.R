library(readxl)
Empang_2001 <- read_excel("~/R/hujan/raw/ground/Empang 1967-2009 43th.xls", 
                          sheet = "2001", range = "A18:M49", na = "-")

t2009 <- as.data.frame(rep("2009",31))

Empang_2009$X__1 <- t2009$`rep("2009", 31)`
Empang_2009 <- gather(Empang_2009, "Bulan", "Hujan", 2:13)
Empang_2009 <- bind_cols(hari, Empang_2009)
Empang_2009 <- unite(Empang_2009,"Hari",hari,Bulan,X__1,sep = "-")

empang <- bind_rows(Empang_2000,Empang_2001,Empang_2002,Empang_2003,Empang_2004,Empang_2005,Empang_2006,Empang_2007,Empang_2008,Empang_2009)

depok<-depok[rowSums(is.na(depok[ ,1]))==0, ]