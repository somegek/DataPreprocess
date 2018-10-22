getNewWeights_truncate <- function(DT){
  
  # get truncated weights
  DT_W <- getNewWeights(DT)
  
  # get forecasts and the insample mse
  DT_FCT <- getForecast(DT_W)
  DT_RES <- getEval_insample(DT_FCT)
  
  # get position of lowest ratio
  findMin <- function(ratio,thres){
    thres[ratio==min(ratio)]
  }
  DT_THRES <- DT_RES[, findMin(RATIO_SUB_THRES,THRESHOLD), by= c('FCT_TOPIC','FCT_HORIZON', 'TEST_PERIOD')]
  setnames(DT_THRES, old ='V1', new = 'FCT_THRES')
  DT_new <- merge(DT,DT_THRES,by = c('FCT_TOPIC','FCT_HORIZON','TEST_PERIOD'))
  DT_new <- DT_new[,.(FCT_TOPIC,FCT_HORIZON,FCT_THRES,FCT_SOURCE,TEST_PERIOD,TIME_PERIOD,TRUE_VALUE,OBS_VALUE,WEIGHT_EQUAL,WEIGHT_SUB,WEIGHT_FULL)]
  
  # remove value below threshold and scale to 1
  thresholdFunc <- function(x, threshold){
    x[x<threshold] <- 0
    x <- x/sum(x)
    x
  }
  
  # make a column indicating the threshold for the given rows
  DT_new[, THRESHOLD:=FCT_THRES]
  
  # take sub and scale the results with threshold
  weightsList <- unique(grep('SUB', names(DT_new), value = TRUE))
  newWeightsList <- paste0(weightsList,'_THRES')
  DT_new[, (newWeightsList) := lapply(.SD, function(x) thresholdFunc(x, THRESHOLD)), .SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD', 'TEST_PERIOD', 'THRESHOLD')]
  # DT[, (weightsList) := NULL]
  
  # take full and remove the effect of different time period
  DT_FULL <- unique(DT_new[,.(FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD, WEIGHT_FULL)])
  
  # scale the results with threshold then remove the extra column to merge
  weightsList <- unique(grep('FULL', names(DT_FULL), value = TRUE))
  newWeightsList <- paste0(weightsList,'_THRES')
  DT_FULL[, (newWeightsList) := lapply(.SD, function(x) thresholdFunc(x, THRESHOLD)), .SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON', 'TEST_PERIOD', 'THRESHOLD')]
  DT_FULL[, (weightsList) := NULL]
  # DT[, (weightsList) := NULL]
  
  # merge data
  setkey(DT_FULL, FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD)
  setkey(DT_new, FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TEST_PERIOD, THRESHOLD)
  DT_new <- merge(DT_new,DT_FULL)
  
  DT_new
}