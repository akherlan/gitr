# Kasus: Read multiple xlsx files with multiple sheets into one R data frame

require(xlsx)    
file.list <- list.files(recursive=T,pattern='*.xlsx')  # get files list from folder

for (i in 1:length(files.list)){                                           
  wb <- loadWorkbook(files.list[i])           # select a file & load workbook
  sheet <- getSheets(wb)                      # get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp<-read.xlsx(files.list[i], sheetIndex=j, colIndex= c(1:6,8:10,12:17,19),
                   sheetName=NULL, startRow=4, endRow=NULL,
                   as.data.frame=TRUE, header=F)   
    if (i==1&j==1) dataset<-tmp else dataset<-rbind(dataset,tmp)   #happend to previous
    
  }
}

# source: https://stackoverflow.com/questions/38197705/read-multiple-xlsx-files-with-multiple-sheets-into-one-r-data-frame

# another