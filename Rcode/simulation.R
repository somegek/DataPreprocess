# setting: try to see how the variance react when the threshold is away from the true value
source('~/Github/DataPreprocess/Rcode/global.R', echo=FALSE)
set.seed(12345677)

###tuning parameter
obsAmount <- 10
trueWeight <- c(-0.5, 0.3, 0, 1.2)
trueWeight <- trueWeight/sum(trueWeight)
timeseriesAmount <- length(trueWeight)

trueMean <- rep(0,timeseriesAmount)
trueVariance <- matrix(0.8, ncol = timeseriesAmount, nrow = timeseriesAmount)
iota <- rep(1,timeseriesAmount)
diag(trueVariance) <- iota
# > trueVariance
#      [,1] [,2] [,3] [,4]
# [1,]  1.0  0.8  0.8  0.8
# [2,]  0.8  1.0  0.8  0.8
# [3,]  0.8  0.8  1.0  0.8
# [4,]  0.8  0.8  0.8  1.0


truncation <- c(-Inf,seq(from = -1, by = 0.1, to = 0))
# > truncation
# [1] -Inf -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0
truncIota <- rep(1,length(truncation))
truncVal <- 0

epsMean <- 0
epsVariance <- 0.6 #0.4, 0.5, 0.6, 0.7, 0.8, 0.9


###code
DF <- mvrnorm(n = obsAmount, trueMean, trueVariance)
eps <- rnorm(n = obsAmount, epsMean, epsVariance)

trueValue <- DF %*% trueWeight
obsValue <- trueValue + eps
errValue <- DF-obsValue%*%iota
covar <- nearPD(cov(errValue))$mat

weight <- solve(covar,iota)
weight <- weight/sum(weight)

wmat <- weight%*%truncIota
tmat <- matrix(rep(truncation,timeseriesAmount),nrow = timeseriesAmount, byrow = T)

wmatTrunc <- wmat
wmatTrunc[wmatTrunc<tmat] <- truncVal
scaling <- matrix(rep(colSums(wmatTrunc),timeseriesAmount), nrow = timeseriesAmount, byrow = T)
wmatTrunc <- wmatTrunc/scaling
wmatTrunc

fcst <- DF%*%wmatTrunc

fcsterr <- trueValue%*% truncIota - fcst
fcstmse <- colMeans(fcsterr^2)
fcstmse
plot(fcstmse, type = 'l')