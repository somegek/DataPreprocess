getForecast_truncate <- function(DT){
  # this function is similar to getForecast()
  # it does the same thing. The only difference is line 17 with setkey.
  # since there are only 1 threshold per test period, ordering on threshold does not make sense anymore
  
  # get the lists of weights, and forecasts
  weightsList <- grep('WEIGHT',names(DT), value = TRUE)
  methods2Combine <- str_sub(weightsList, start = 8)
  forecastNameList <- paste0('FCT_', methods2Combine)
  
  # make the forcast with sum(weights*obs_value)
  DT[,(forecastNameList) := lapply(.SD, function(x)  sum(x*OBS_VALUE)),.SDcols = weightsList, by=c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD', 'TEST_PERIOD', 'THRESHOLD')]
  
  # take 1 value per time period
  DT_FCT <- unique(DT[,c('FCT_TOPIC', 'FCT_HORIZON', 'THRESHOLD', 'TIME_PERIOD', 'TEST_PERIOD', 'TRUE_VALUE',forecastNameList),with = FALSE])
  
  setkey(DT_FCT,FCT_TOPIC,FCT_HORIZON,TEST_PERIOD)
  
  DT_FCT
}