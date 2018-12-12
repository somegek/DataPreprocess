startThres <- -5
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = 0.1)

plotData <- function(DT,legendPos){
  plot(DT$RATIO_FULL_THRES,
       x =  thresholdList,
       type = 'l', ylab = 'Ratio', xlab='Truncation', main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  legend(legendPos,c("MSPE Ratio"),
         fill=c("black")#, bty = 'n'
  )
  # if(DT$FCT_HORIZON[1]==1)  axis(1, at=seq(2,74,8), labels=seq(2000,2018,2)) else   axis(1, at=seq(1,75,8), labels=seq(2000,2018,2)) 
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

