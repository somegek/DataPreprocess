# steps in this project explained
# step 1: preprocess the data to clean up missing values and calculate covariance
# step 2: use the covariance to calculate the weights without threshold
# step 3: set threshold for the weights
# step 4: use the combine the forecasts using the weights
# step 5: evaluate forcasts


#####################################################################################################################
################################################### before running ##################################################
#####################################################################################################################
### save data.csv downloaded in the links in preprocess to SPF.csv, UNEM.csv, HICP.csv, and RGDP.csv in Input/
### remove the first 4 rows in UNEM.csv, HICP.csv, and RGDP.csv


source('Rcode/global.R', echo=FALSE)

########## step 1: preprocess the data to clean up missing values and calculate covariance #########
# uncomment to rerun
source('Rcode/preprocess.R', echo=FALSE)


########## step 2: use the covariance to calculate the weights without threshold #########
# uncomment to rerun
load(file = "Input/preprocessed.RData")
DT <- getWeights(DT)
save(DT, file = 'Input/Initial_Weights.RData')
# when TIME_PERIOD < TEST_PERIOD, it is the training data set
# when TIME_PERIOD == TEST_PERIOD, it is the testing data set

########## step 3: set threshold for the weights #########
#sequence from -1 to 0 with step size 0.1
startThres <- -1
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = (endThres-startThres)/10)
thresholdList <- c(round(thresholdList,2), -Inf)

# load data
load(file = 'Input/Initial_Weights.RData')
# get the new weights given the threshold
DT <- getNewWeights(DT)
# save data
save(DT, file = 'Input/New_Weights.RData')


########## step 4: use the combine the forecasts using the weights #########
# get the forecast value
load(file = 'Input/New_Weights.RData')
DT_FCT <- getForecast(DT)
save(DT_FCT, file = 'Input/Forecasts.RData')
########## step 5: evaluate forcasts #########
RES <- getEval(DT_FCT)
RES[,THRESHOLD := paste0("\"",THRESHOLD,"\"")]
numericList <- c('RATIO_SUB_THRES', 'RATIO_FULL_THRES')
RES[, (numericList) := lapply(.SD, function(x) round(x,2)), .SDcols = numericList]
print(RES)
write.csv(RES, file = 'Output/Preliminary.csv',row.names = F)

RESSummary <- DT_FCT[, lapply(.SD,summary),.SDcols = c('ERR_EQUAL','ERR_SUB_THRES'),by=c('FCT_TOPIC','FCT_HORIZON','THRESHOLD')]
RESSummary[,Statistics:=c('min','1st Q', 'median','mean','3rd Q','max')]
RESIQR <- DT_FCT[, lapply(.SD,IQR),.SDcols = c('ERR_EQUAL','ERR_SUB_THRES'),by=c('FCT_TOPIC','FCT_HORIZON','THRESHOLD')]


###########
# load data
load(file = 'Input/Initial_Weights.RData')
# get the new weights given the threshold
DT <- getNewWeights_truncate(DT)
DT_FCT <- getForecast_truncate(DT)
RES <- getEval_truncate(DT_FCT)
numericList <- c('RATIO_SUB','RATIO_SUB_THRES', 'RATIO_FULL','RATIO_FULL_THRES')
RES[, (numericList) := lapply(.SD, function(x) round(x,2)), .SDcols = numericList]
print(RES)
write.csv(RES, file = 'Output/OOS_Truncate.csv',row.names = F)


RESSummary <- DT_FCT[, lapply(.SD,summary),.SDcols = c('ERR_EQUAL','ERR_SUB_THRES'),by=c('FCT_TOPIC','FCT_HORIZON')]
RESSummary[,Statistics:=c('min','1st Q', 'median','mean','3rd Q','max')]
RESIQR <- DT_FCT[, lapply(.SD,IQR),.SDcols = c('ERR_EQUAL','ERR_SUB_THRES'),by=c('FCT_TOPIC','FCT_HORIZON')]
