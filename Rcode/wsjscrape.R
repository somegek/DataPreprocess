# http://online.wsj.com/public/resources/documents/wsjecon0918.xls
for (yr in 2:18){
  for (mn in 1:12){
    
    if(yr == 2 & mn != 12) next
    if(yr == 18 & mn > 10) next
    yr <- as.character(yr)
    mn <- as.character(mn)
    
    if(nchar(yr) == 1) yr <- paste0('0',yr)
    if(nchar(mn) == 1) mn <- paste0('0',mn)
    mnyr <- paste0(mn,yr)
    
    download.file(paste0('http://online.wsj.com/public/resources/documents/wsjecon',mnyr,'.xls'), destfile = paste0('WSJData/wsjecon',mnyr,'.xls'), method = 'curl')
  }
}