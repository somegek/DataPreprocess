# this script plot the mspe ratio for different time series
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
  plot(DT$RATIO_FULL_THRES,
       x =  thresholdList,
       type = 'l', ylab = 'Ratio', xlab='Truncation', main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  legend(legendPos,c("MSPE Ratio"),
         fill=c("black")#, bty = 'n'
  )
}

makePlot <- function(){
  par(mfrow=c(3,2))
  plotData(RES[FCT_TOPIC=='HICP' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='HICP' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='RGDP' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='RGDP' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='UNEM' & FCT_HORIZON==1 & THRESHOLD!='-Inf'],"topright")
  plotData(RES[FCT_TOPIC=='UNEM' & FCT_HORIZON==2 & THRESHOLD!='-Inf'],"topright")
}

makePlot()

