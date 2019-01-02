load(file = "Input/preprocessed.RData")
# this script make a summary of the correlation matrix


# get the weights by first getting the covariance, then calculate the weights
getCorr<- function(tempDT){
  # list of all forecasters
  forcastersList <- unique(tempDT$FCT_SOURCE)
  amountOfForecasters <- length(forcastersList)
  
  # allocate NA covariance
  tempCov <- matrix(nrow = amountOfForecasters, ncol = amountOfForecasters)
  
  # covariance is calculated with trainDT
  # for each element in covariance
  for (i in 1:amountOfForecasters) {
    for (j in 1:amountOfForecasters) {
      
      # get error per forcaster
      ei <- tempDT[FCT_SOURCE == forcastersList[[i]], .(TIME_PERIOD, ERR_VALUE)]
      ej <- tempDT[FCT_SOURCE == forcastersList[[j]], .(TIME_PERIOD, ERR_VALUE)]
      setkey(ei, TIME_PERIOD)
      setkey(ej, TIME_PERIOD)
      
      # get intersect on TIME_PERIOD
      e <- ei[ej, nomatch = 0]
      
      # get covariance
      if (nrow(e) == 0) {
        tempCov[[i, j]] <- 0
      } else {
        tempCov[[i, j]] <- sum(e$ERR_VALUE * e$i.ERR_VALUE) / nrow(e)
      }
    }
  }
  # make sure it is p.d. (choice according to the paper)
  tempCov <- nearPD(tempCov)
  corr <- cov2cor(tempCov$mat)
  corr
}

# do getCorr for all topic and horizon
corrList <- list()
i <- 1
for (topic in c('HICP','RGDP','UNEM')) {
  for (horz in 1:2){
    
    corr <- getCorr(DT[FCT_TOPIC==topic & FCT_HORIZON==horz])
    corrList[[i]] <- corr
    i <- i + 1
  }
}

# make a column, remove correlation == 1 (diagonal element)
# then take summary of it
# lapply to all cov in cov list
corrsummary <- as.data.table(lapply(corrList, function(x) round(summary(matrix(x[row(x)!=col(x)],ncol=1)[TRUE]),2)))
corrsummary[,Statistics:=c('min','1st Q', 'median','mean','3rd Q','max')]
setnames(corrsummary, paste0('V',1:6), c('HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2'))
setcolorder(corrsummary, c('Statistics', 'HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2' ))
write.csv(corrsummary, file='Output/corrSummary.csv', row.names = F)


