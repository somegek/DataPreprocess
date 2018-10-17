getNewWeights <- function(DT){
  
  # remove value below threshold and scale to 1
  thresholdFunc <- function(x, threshold){
    x[x<threshold] <- 0
    x <- x/sum(x)
    x
  }
  
  # remove value below threshold
  thresholdFunc2 <- function(x, threshold){
    x[x<threshold] <- 0
    x
  }
  
  # make a column indicating the threshold for the given rows
  thresholdColumn <- sort(rep(thresholdList, nrow(DT)))
  DT <- data.table(DT,THRESHOLD = thresholdColumn)
  
  # take sub and scale the results with threshold
  weightsList <- unique(grep('SUB', names(DT), value = TRUE))
  newWeightsList <- paste0(weightsList,'_THRES')
  DT[, (newWeightsList) := lapply(.SD, function(x) thresholdFunc(x, THRESHOLD)), .SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD', 'TEST_PERIOD', 'THRESHOLD')]
  DT[, (weightsList) := NULL]
  
  # take full and remove the effect of different time period
  DT_FULL <- unique(DT[,.(FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD, WEIGHT_FULL)])
  
  # scale the results with threshold then remove the extra column to merge
  weightsList <- unique(grep('FULL', names(DT_FULL), value = TRUE))
  newWeightsList <- paste0(weightsList,'_THRES')
  DT_FULL[, (newWeightsList) := lapply(.SD, function(x) thresholdFunc(x, THRESHOLD)), .SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON', 'TEST_PERIOD', 'THRESHOLD')]
  DT_FULL[, (weightsList) := NULL]
  DT[, (weightsList) := NULL]
  
  # merge data
  setkey(DT_FULL, FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD)
  setkey(DT, FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD)
  DT <- merge(DT,DT_FULL)
  
  DT
}