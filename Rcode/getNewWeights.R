getNewWeights <- function(DT){
  # this function get the truncated weight from the optimal weight
  # input DT with optimal weight
  # output DT with threshold weight
  
  # remove value below threshold and scale to 1
  thresholdFunc <- function(x, threshold){
    x[x<threshold] <- 0
    x <- x/sum(x)
    x
  }
  
  # make a column indicating the threshold for the given rows
  thresholdColumn <- sort(rep(thresholdList, nrow(DT)))
  DT <- data.table(DT,THRESHOLD = thresholdColumn)
  
  # take sub and scale the results with threshold
  weightsList <- unique(grep('WEIGHT', names(DT), value = TRUE))
  newWeightsList <- paste0(weightsList,'_THRES')
  DT[, (newWeightsList) := lapply(.SD, function(x) thresholdFunc(x, THRESHOLD)), .SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD', 'TEST_PERIOD', 'THRESHOLD')]
  DT[, WEIGHT_EQUAL_THRES := NULL]
  
  DT
}