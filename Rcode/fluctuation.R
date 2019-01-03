load(file = 'Input/Initial_Weights.RData')
source('./Rcode/global.R', echo=F)
# this script see how the threshold varies
# output csv threshold summary
# table 11

#sequence from -10 to 0 with step size 0.1
startThres <- -10
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = 0.1)
# add no threshold
thresholdList <- c(round(thresholdList,2))#, -Inf)

# get truncated weights
DT_W <- getNewWeights(DT)

# get forecasts and the insample mse
DT_FCT <- getForecast(DT_W)
DT_RES <- getEval_insample(DT_FCT)

# get position of lowest ratio
findMin <- function(ratio,thres){
  first(thres[ratio==min(ratio)])
}
# get the threshold where the ratio is minimum
DT_THRES <- DT_RES[, findMin(RATIO_SUB_THRES,THRESHOLD), by= c('FCT_TOPIC','FCT_HORIZON', 'TEST_PERIOD')]
setnames(DT_THRES, old ='V1', new = 'FCT_THRES')
# make summary
thresSummary <- DT_THRES[,.(Minimum = min(FCT_THRES), First = quantile(FCT_THRES,0.25), Mean = round(mean(FCT_THRES),1), Median = quantile(FCT_THRES,0.5), Third = quantile(FCT_THRES,0.75), IQR = IQR(FCT_THRES), Maximum = max(FCT_THRES)), by = c('FCT_TOPIC','FCT_HORIZON')]
# thresSummary[, (c('Minimum','First', 'Mean', 'Median', 'Third', 'IQR', 'Maximum')):= lapply(.SD, function(x) paste0("\"",round(x,2),"\"")), .SDcols = c('Minimum','First', 'Mean', 'Median', 'Third', 'IQR', 'Maximum')]
write.csv(thresSummary, file = 'Output/thresholdSummary.csv',row.names = F)
