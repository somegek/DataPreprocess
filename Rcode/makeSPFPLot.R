source('Rcode/global.R', echo=FALSE)

load(file = "Input/preprocessed.RData")

DT[, MAX := max(OBS_VALUE), by = c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]
DT[, MIN := min(OBS_VALUE), by = c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]
DT[, MEAN := mean(OBS_VALUE), by = c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]

DT_new <- unique(DT[,.(FCT_TOPIC,FCT_HORIZON,TIME_PERIOD,MAX,MIN,MEAN,TRUE_VALUE)])
setkey(DT_new, FCT_TOPIC, FCT_HORIZON, TIME_PERIOD)

plotData <- function(DT, ylimpara,legendPos){
  plot(DT$MAX, type = 'l',ylim=ylimpara, ylab = '', xlab='year',xaxt = "n", main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  lines(DT$MIN, type = 'l')
  lines(DT$MEAN, type = 'l', col = 'blue')
  lines(DT$TRUE_VALUE, type = 'l', col = 'red')
  legend(legendPos,c("Max","Min","Mean","TRUE"),
         fill=c("black","black","blue","red")
  )
  if(DT$FCT_HORIZON[1]==1)  axis(1, at=seq(2,74,8), labels=seq(2000,2018,2)) else   axis(1, at=seq(1,75,8), labels=seq(2000,2018,2)) 
}

makePlot <- function(){
par(mfrow=c(3,2))
plotData(DT_new[FCT_TOPIC=='HICP' & FCT_HORIZON==1],c(-1,5),"topleft")
plotData(DT_new[FCT_TOPIC=='HICP' & FCT_HORIZON==2],c(-1,5),"topleft")
plotData(DT_new[FCT_TOPIC=='RGDP' & FCT_HORIZON==1],c(-5,8),"bottomleft")
plotData(DT_new[FCT_TOPIC=='RGDP' & FCT_HORIZON==2],c(-5,8),"bottomleft")
plotData(DT_new[FCT_TOPIC=='UNEM' & FCT_HORIZON==1],c(5,14),"bottomleft")
plotData(DT_new[FCT_TOPIC=='UNEM' & FCT_HORIZON==2],c(5,14),"bottomleft")
}

makePlot()

