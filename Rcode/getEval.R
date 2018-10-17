getEval <- function(DT_FCT){
  # get list of forecasts, without FCT_TOPIC and FCT_HORIZON
  forecastList <- grep('FCT_',names(DT_FCT),value = TRUE)[-1:-2]
  
  # calculate error
  errList <- paste0('ERR_',str_sub(forecastList,start = 5))
  DT_FCT[, (errList) := lapply(.SD, function(x) TRUE_VALUE - x), .SDcols = forecastList]
  
  # calculate error squared
  errSqList <- paste0(errList,"_SQ")
  DT_FCT[, (errSqList) := lapply(.SD, function(x) x^2), .SDcols = errList]
  
  # calculated sum of error squared
  errSumSqList <- paste0(errList,"_SUM_SQ")
  DT_FCT[TIME_PERIOD == TEST_PERIOD, (errSumSqList) := lapply(.SD, function(x) sum(x)), .SDcols = errSqList, by = c('FCT_TOPIC', 'FCT_HORIZON', 'THRESHOLD')]
  
  # calculate the ratio to the equal weights
  # the ratio is defined as SSE of full and sub divided by the SSE of equal weights
  ratioSumSqList <- paste0('RATIO_',str_sub(forecastList,start = 5))
  DT_FCT[, (ratioSumSqList) := lapply(.SD, function(x) x/ERR_EQUAL_SUM_SQ), .SDcols = errSumSqList]
  
  # show the ratio and SSE of equal weights per category
  DT_RES <- unique(DT_FCT[,.(FCT_TOPIC, FCT_HORIZON, TEST_PERIOD, THRESHOLD, ERR_EQUAL_SUM_SQ, RATIO_SUB_THRES, RATIO_FULL_THRES)])
  DT_RES
}