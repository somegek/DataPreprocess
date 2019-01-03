# this script produces the main results in constant truncation and optimal truncation.
# steps in this project explained
# step 1: preprocess the data to clean up missing values and calculate covariance
# step 2: use the covariance to calculate the weights without threshold
# step 3: set threshold for the weights
# step 4: use the combine the forecasts using the weights
# step 5: evaluate forcasts

# table 7,8,9,10,12,13,14

#####################################################################################################################
################################################### before running ##################################################
#####################################################################################################################
### save data.csv downloaded in the links in preprocess to SPF.csv, UNEM.csv, HICP.csv, and RGDP.csv in Input/
### remove the first 4 rows in UNEM.csv, HICP.csv, and RGDP.csv

# load the functions and packages. If the package does not exist, install them.
source('Rcode/global.R', echo=FALSE)

########## step 1: preprocess the data to clean up missing values and calculate covariance #########
# uncomment to rerun
# source('Rcode/preprocess.R', echo=FALSE)


########## step 2: use the covariance to calculate the weights without threshold #########
# uncomment to rerun
# load(file = "Input/preprocessed.RData")
# DT <- getWeights(DT)
# save(DT, file = 'Input/Initial_Weights.RData')
# when TIME_PERIOD < TEST_PERIOD, it is the training data set
# when TIME_PERIOD == TEST_PERIOD, it is the testing data set

########## step 3: set threshold for the weights #########
# sequence from -5 to 0 with step size 0.1
startThres <- -5
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = 0.5)
# append -inf in behind
thresholdList <- c(thresholdList, -Inf)

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
# get evaluation results
load(file='Input/Forecasts.RData')
RES <- getEval(DT_FCT)
# paste "" to avoid error in excel
RES[,THRESHOLD := paste0("\"",THRESHOLD,"\"")]
numericList <- c('RATIO_SUB_THRES', 'RATIO_FULL_THRES','RATIO_SUB_THRES_ABS', 'RATIO_FULL_THRES_ABS')
# round to 2 digits
RES[, (numericList) := lapply(.SD, function(x) round(x,4)), .SDcols = numericList]
print(RES)
# save
write.csv(RES, file = 'Output/Preliminary.csv',row.names = F)

###########
startThres <- -1
endThres <- 0
thresholdList <- seq(from = startThres, to = endThres, by = 0.1)
# append -inf in behind
thresholdList <- c(thresholdList, -Inf)
# load data
load(file = 'Input/Initial_Weights.RData')
# get the new weights with optimal threshold
DT <- getNewWeights_truncate(DT)
# get forecast of it
DT_FCT <- getForecast_truncate(DT)
# evaluate it
RES <- getEval_truncate(DT_FCT)
# round to 2 digit
numericList <- c('RATIO_SUB','RATIO_SUB_THRES', 'RATIO_FULL','RATIO_FULL_THRES','RATIO_SUB_ABS','RATIO_FULL_ABS','RATIO_SUB_THRES_ABS', 'RATIO_FULL_THRES_ABS')
RES[, (numericList) := lapply(.SD, function(x) round(x,4)), .SDcols = numericList]
print(RES)
write.csv(RES, file = 'Output/OOS_Truncate.csv',row.names = F)