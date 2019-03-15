# test the difference between threshold and optimization
if(!require(quadprog))install.packages('quadprog')
library(quadprog)
source('Rcode/global.R', echo=FALSE)

load(file = 'Input/New_Weights.RData')

calculateWeightCon<- function(Dmat,thres){
  FCT_SOURCE <- names(Dmat)
  Dmat <- as.matrix(Dmat)
  n <- nrow(Dmat)
  dvec <- rep(0,n)
  Amat <- t(rbind(1,diag(1, nrow = n, ncol = n)))
  bvec <- c(1,rep(thres,n))
  
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq=1, factorized=FALSE)
  solution <- sol$solution
  names(solution) <- FCT_SOURCE
  solution
}

# calcualte the covariance matrix
getCovariance <- function(tempDT, trainDT) {
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
      ei <- trainDT[FCT_SOURCE == forcastersList[[i]], .(TIME_PERIOD, ERR_VALUE)]
      ej <- trainDT[FCT_SOURCE == forcastersList[[j]], .(TIME_PERIOD, ERR_VALUE)]
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
  
  # take the matrix from the output structure of nearPD
  tempCov <- as.data.frame.matrix(tempCov$mat)
  colnames(tempCov) <- forcastersList
  tempCov
}

# get the sub covariance matrix given name list
getSubCov <- function(Cov, colNameList) {
  colInd <- which(colnames(Cov) %in% colNameList)
  Cov[colInd, colInd]
}

getWeightsCon <- function(tempDT, trainDT, threshold){
  # get covariance
  tempCov <- getCovariance(tempDT, trainDT)
  # calculate the constrainted weights
  tempWeight <- calculateWeightCon(getSubCov(tempCov, tempDT[TIME_PERIOD == last(sort(TIME_PERIOD)), FCT_SOURCE]),threshold)
  # fill in the weights
  tempWeightDT <- as.data.table(tempWeight, keep.rownames = "FCT_SOURCE")
  setkey(tempWeightDT, FCT_SOURCE)
  setkey(tempDT, FCT_SOURCE)
  tempDT <- merge(tempDT, tempWeightDT, all.x = TRUE)
  tempDT[is.na(tempWeight), tempWeight := 0]
  return(tempDT$tempWeight)
}

# -inf won't work in quadprog
DT[is.infinite(THRESHOLD), THRESHOLD:=-99999999]
setkey(DT,FCT_TOPIC,FCT_HORIZON,TEST_PERIOD,THRESHOLD,FCT_SOURCE)
DT[, WEIGHT_CON := getWeightsCon(.SD[TRUE], .SD[TIME_PERIOD < TEST_PERIOD], THRESHOLD), by = c("FCT_TOPIC", "FCT_HORIZON", "TEST_PERIOD", "THRESHOLD")]
DT <- DT[TIME_PERIOD==TEST_PERIOD]
DT[, PERC_WEIGHT_DIFF:=(WEIGHT_SUB-WEIGHT_CON)]
SUB_SUMMARY <- dcast(DT[,.(Statistics = 1:6,V=summary(WEIGHT_SUB)),by=TOPIC_HORIZON],Statistics~TOPIC_HORIZON, value.var = 'V')
CON_SUMMARY <- dcast(DT[,.(Statistics = 1:6,V=summary(WEIGHT_CON)),by=TOPIC_HORIZON],Statistics~TOPIC_HORIZON, value.var = 'V')
DIFF_SUMMARY <- dcast(DT[,.(Statistics = 1:6,V=summary(PERC_WEIGHT_DIFF)),by=TOPIC_HORIZON],Statistics~TOPIC_HORIZON, value.var = 'V')
SUB_SUMMARY[,Statistics:=c('Min','1st Q','Mean','Median','3rd Q','Max')]
CON_SUMMARY[,Statistics:=c('Min','1st Q','Mean','Median','3rd Q','Max')]
DIFF_SUMMARY[,Statistics:=c('Min','1st Q','Mean','Median','3rd Q','Max')]
save(DT,SUB_SUMMARY,CON_SUMMARY,DIFF_SUMMARY, file = 'Output/optimTest.RData')
