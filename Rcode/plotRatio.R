# this script plot the mspe ratio for different time series
# figure 2 and figure 4
startThres <- -5
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = 0.1)


# load data
load(file = 'Input/Initial_Weights.RData')
# get the new weights given the threshold and get the results
DT <- getNewWeights(DT)
DT_FCT <- getForecast(DT)
RES <- getEval(DT_FCT)

# plot the data
plotData <- function(DT,legendPos){
  # uncomment to change values
  # bias
  t1 <- DT$RATIO_FULL_THRES
  t2 <- DT$RATIO_FULL_THRES_ABS
  # no bias
  # t1 <- DT$RATIO_SUB_THRES
  # t2 <- DT$RATIO_SUB_THRES_ABS
  plot(t1,
       x =  thresholdList,
       type = 'l', ylab = 'Ratio', ylim =range(t1,t2), xlab='Truncation', main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  lines(t2,
       x =  thresholdList, col = 'red')
  legend(legendPos,c("MSPE Ratio",'MAPE Ratio'),
         fill=c("black",'red')
  )
}

makePlot <- function(){
  par(mfrow=c(3,2))
  plotData(RES[FCT_TOPIC=='HICP' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"bottomleft")
  plotData(RES[FCT_TOPIC=='HICP' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='RGDP' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='RGDP' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='UNEM' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='UNEM' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
}

makePlot()

