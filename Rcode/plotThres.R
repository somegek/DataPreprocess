source('~/Erasmus/Msc QF/Thesis/Code/DataPreprocess/Rcode/fluctuation.R', echo=TRUE)
# this script plot the selected threshold
# figure 3 threshold

# make plot
DT_THRES[,FCT_MED:=median(FCT_THRES),by=c('FCT_TOPIC','FCT_HORIZON')]
DT_THRES[is.infinite(FCT_THRES), FCT_THRES:=FCT_MED]
plotData <- function(DT,legendPos){
  plot(y = DT$FCT_THRES, x= DT$TEST_PERIOD,
       type = 'l', ylab = 'Threshold', xlab='year', main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  legend(legendPos,c("Selected Threshold"),
         fill=c("black")#, bty = 'n'
  )
}

makePlot <- function(){
  par(mfrow=c(3,2))
  plotData(DT_THRES[FCT_TOPIC=='HICP' & FCT_HORIZON==1],"bottomleft")
  plotData(DT_THRES[FCT_TOPIC=='HICP' & FCT_HORIZON==2],"bottomright")
  plotData(DT_THRES[FCT_TOPIC=='RGDP' & FCT_HORIZON==1],"bottomleft")
  plotData(DT_THRES[FCT_TOPIC=='RGDP' & FCT_HORIZON==2],"bottomleft")
  plotData(DT_THRES[FCT_TOPIC=='UNEM' & FCT_HORIZON==1],"bottomright")
  plotData(DT_THRES[FCT_TOPIC=='UNEM' & FCT_HORIZON==2],"bottomleft")
}

makePlot()

