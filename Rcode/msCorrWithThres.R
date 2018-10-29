load(file = "Input/preprocessed.RData")
source('~/Github/DataPreprocess/Rcode/global.R', echo=F)

DT_MS <- unique(DT[, .(TRUE_VALUE, MIN_VALUE = min(OBS_VALUE), MAX_VALUE = max(OBS_VALUE)), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TIME_PERIOD')])

DT_MS[TRUE_VALUE<MIN_VALUE | TRUE_VALUE>MAX_VALUE, Outside := 1]
DT_MS[TRUE_VALUE>=MIN_VALUE & TRUE_VALUE<=MAX_VALUE, Outside := 0]

load(file = 'Input/Initial_Weights.RData')

#sequence from -1 to 0 with step size 0.1
startThres <- -1
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = (endThres-startThres)/10)
thresholdList <- c(round(thresholdList,2), -Inf)

# get truncated weights
DT_W <- getNewWeights(DT)

# get forecasts and the insample mse
DT_FCT <- getForecast(DT_W)
DT_RES <- getEval_insample(DT_FCT)

# get position of lowest ratio
findMin <- function(ratio,thres){
  last(thres[ratio==min(ratio)])
}

DT_THRES <- DT_RES[, findMin(RATIO_SUB_THRES,THRESHOLD), by= c('FCT_TOPIC','FCT_HORIZON', 'TEST_PERIOD')]
setnames(DT_THRES, old ='V1', new = 'FCT_THRES')

for(topic in c('HICP','RGDP','UNEM')){
  for(horz in 1:2){
    testList <- DT_THRES[FCT_TOPIC==topic & FCT_HORIZON == horz, TEST_PERIOD]
    tempDT <- DT_MS[FCT_TOPIC==topic & FCT_HORIZON == horz]
    for (test_time in testList){
      DT_THRES[FCT_TOPIC==topic & FCT_HORIZON == horz & TEST_PERIOD == test_time, avgOutside := mean(tempDT[TIME_PERIOD < test_time,Outside])]
    }
  }
}

DT_THRES <- DT_THRES[is.finite(FCT_THRES)]

corSummary <- DT_THRES[,.(Correlation=round(cor(FCT_THRES,avgOutside),2)), by = c('FCT_TOPIC','FCT_HORIZON')]

write.csv(corSummary, file = 'Output/corrSummary.csv',row.names = F)
