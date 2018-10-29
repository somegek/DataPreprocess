load(file = 'Input/Initial_Weights.RData')
source('~/Github/DataPreprocess/Rcode/global.R', echo=F)

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
thresSummary <- DT_THRES[,.(Minimum = min(FCT_THRES), First = quantile(FCT_THRES,0.25), Mean = mean(FCT_THRES), Median = quantile(FCT_THRES,0.5), Third = quantile(FCT_THRES,0.75), IQR = IQR(FCT_THRES), Maximum = max(FCT_THRES)), by = c('FCT_TOPIC','FCT_HORIZON')]
thresSummary[, (c('Minimum','First', 'Mean', 'Median', 'Third', 'IQR', 'Maximum')):= lapply(.SD, function(x) paste0("\"",round(x,2),"\"")), .SDcols = c('Minimum','First', 'Mean', 'Median', 'Third', 'IQR', 'Maximum')]
write.csv(thresSummary, file = 'Output/thresholdSummary.csv',row.names = F)
