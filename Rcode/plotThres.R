DT_THRES[is.infinite(FCT_THRES), FCT_THRES:=-0.9]
plotData <- function(DT,legendPos){
  plot(y = DT$FCT_THRES, x= DT$TEST_PERIOD,
       type = 'l', ylab = 'Threshold', xlab='year', main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  legend(legendPos,c("Selected Threshold"),
         fill=c("black")#, bty = 'n'
  )
  # if(DT$FCT_HORIZON[1]==1)  axis(1, at=seq(2,74,8), labels=seq(2000,2018,2)) else   axis(1, at=seq(1,75,8), labels=seq(2000,2018,2)) 
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

