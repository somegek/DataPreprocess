# setting: try to see how the variance react when the threshold is away from the true value
# this simulation look at difference in signal to noise ratio and correlation
# figure 5 simulation
source('Rcode/global.R', echo=FALSE)
set.seed(12345677)
par(mfrow=c(2,2))

###tuning parameter
obsAmount <- 10
trueWeight <- c(-0.5, 0.3, 0, 1.2)
trueWeight <- trueWeight/sum(trueWeight)
timeseriesAmount <- length(trueWeight)

# start simulation
DT <- data.table()
for (t in 1:2000){
  for (corr in c(0.5,0.6,0.7,0.8)){
    epsList <- c(0.4,0.5,0.6,0.7,0.8,0.9)
    for(epsVar in epsList){
      # make cov
      trueMean <- rep(0,timeseriesAmount)
      trueVariance <- matrix(corr, ncol = timeseriesAmount, nrow = timeseriesAmount)
      iota <- rep(1,timeseriesAmount)
      diag(trueVariance) <- iota
      # > trueVariance when corr is 0.8
      #      [,1] [,2] [,3] [,4]
      # [1,]  1.0  0.8  0.8  0.8
      # [2,]  0.8  1.0  0.8  0.8
      # [3,]  0.8  0.8  1.0  0.8
      # [4,]  0.8  0.8  0.8  1.0
      
      
      truncation <- c(-Inf,seq(from = -5, by = 0.1, to = 0))
      # > truncation
      # [1] -Inf -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0
      truncIota <- rep(1,length(truncation))
      truncVal <- 0
      
      epsMean <- 0
      epsVariance <- epsVar#0.6 #0.4, 0.5, 0.6, 0.7, 0.8, 0.9
      
      
      ###code
      # make data
      DF <- mvrnorm(n = obsAmount, trueMean, trueVariance)
      eps <- rnorm(n = obsAmount, epsMean, epsVariance)
      
      trueValue <- DF %*% trueWeight
      obsValue <- trueValue + eps
      errValue <- DF-obsValue%*%iota
      covar <- nearPD(cov(errValue))$mat
      
      # get optimal weight
      weight <- solve(covar,iota)
      weight <- weight/sum(weight)
      
      # make duplicates of weights
      wmat <- weight%*%truncIota
      tmat <- matrix(rep(truncation,timeseriesAmount),nrow = timeseriesAmount, byrow = T)
      
      # truncate the duplicates
      wmatTrunc <- wmat
      wmatTrunc[wmatTrunc<tmat] <- truncVal
      scaling <- matrix(rep(colSums(wmatTrunc),timeseriesAmount), nrow = timeseriesAmount, byrow = T)
      wmatTrunc <- wmatTrunc/scaling
      wmatTrunc
      
      # make forecast
      fcst <- DF%*%wmatTrunc
      
      # calculate mse
      fcsterr <- trueValue%*% truncIota - fcst
      fcstmse <- colMeans(fcsterr^2)
      DT <- rbind(DT, data.table(correlation = corr, errorVar = epsVar, t(fcstmse)))
    }
  }
}
# calculate average mse
DT <- unique(DT[,(paste0("V",1:52)):=lapply(.SD,mean), .SDcols = paste0("V",1:52), by = c('correlation','errorVar')])
for (corr in c(0.5,0.6,0.7,0.8)){
  for(errVar in epsList){
    if(errVar == 0.4){
      # make new plot
      plot(x = truncation, y = unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l',ylim=c(0,0.4), ylab = 'MSPE', xlab='Threshold', main = paste0('Correlation: ',corr))
    }else if(errVar == 0.5){
      # draw new line
      lines(x = truncation, unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l', col='red')
    }else if(errVar == 0.6){
      # draw new line
      lines(x = truncation, unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l', col='orange')
    }else if(errVar == 0.7){
      # draw new line
      lines(x = truncation, unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l', col='blue')
    }else if(errVar == 0.8){
      # draw new line
      lines(x = truncation, unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l', col='forestgreen')
    }else if(errVar == 0.9){
      # draw new line
      lines(x = truncation, unlist(DT[correlation == corr & errorVar==errVar][,-1:-2]), type = 'l', col='purple')
    }
  }
  legend('bottomleft',as.character(epsList),
         fill=c("black","red","orange","blue","forestgreen","purple")
  )
}
