load(file = "Input/preprocessed.RData")
source('Rcode/global.R', echo=F)

DT_MS <- unique(DT[, .(PERCENT_UNDER = sum(OBS_VALUE<TRUE_VALUE)/.N), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TIME_PERIOD')])

load(file = 'Input/Initial_Weights.RData')

DT_NEGW <- unique(DT[,.(PERCENT_NEG = sum(WEIGHT_SUB<0)/.N), by = c('FCT_TOPIC', 'FCT_HORIZON','TIME_PERIOD')])

setkey(DT_MS, FCT_TOPIC ,FCT_HORIZON ,TIME_PERIOD)
setkey(DT_NEGW, FCT_TOPIC ,FCT_HORIZON ,TIME_PERIOD)
DT_FULL <- merge(DT_MS,DT_NEGW)
DT_FULL[,.(correlation=cor(PERCENT_UNDER,PERCENT_NEG)), by= c('FCT_TOPIC', 'FCT_HORIZON')]



plotData <- function(DT,legendPos, ylimpara = c(0,1)){
  plot(DT$PERCENT_UNDER, type = 'l',ylim=ylimpara, ylab = '', xlab='year',xaxt = "n", main = paste0(unique(DT$FCT_TOPIC), " ",unique(DT$FCT_HORIZON)," year ahead"))
  lines(DT$PERCENT_NEG, type = 'l', col = 'red')
  legend(legendPos,c("scaled absolute deviation from 0.5", "% negative weight"),
         fill=c("black","red"), bty = 'n'
  )
  # if(DT$FCT_HORIZON[1]==1)  axis(1, at=seq(2,74,8), labels=seq(2000,2018,2)) else   axis(1, at=seq(1,75,8), labels=seq(2000,2018,2)) 
}

makePlot <- function(){
  par(mfrow=c(3,2))
  plotData(DT_FULL[FCT_TOPIC=='HICP' & FCT_HORIZON==1],"bottomright")
  plotData(DT_FULL[FCT_TOPIC=='HICP' & FCT_HORIZON==2],"bottomright")
  plotData(DT_FULL[FCT_TOPIC=='RGDP' & FCT_HORIZON==1],"bottomright")
  plotData(DT_FULL[FCT_TOPIC=='RGDP' & FCT_HORIZON==2],"bottomright")
  plotData(DT_FULL[FCT_TOPIC=='UNEM' & FCT_HORIZON==1],"bottomright")
  plotData(DT_FULL[FCT_TOPIC=='UNEM' & FCT_HORIZON==2],"bottomright")
}

makePlot()

