getWeights <- function(DT){
  
  # get the weights using simlpe formula
  calculateWeight <- function (Cov){
    weight <- solve(Cov,rep(1,ncol(Cov)))
    weight <- weight / sum(weight)
    weight
  }
  
  
  # get the sub covariance matrix given name list
  getSubCov <- function(Cov,colNameList){
    colInd <- which(colnames(Cov) %in% colNameList)
    Cov[colInd,colInd]
  }
  
  # get the weights by first getting the covariance, then calculate the weights
  getWeightsSubFunc <- function(isFull, tempDT, trainDT){
    # list of all forecasters
    forcastersList <- unique(tempDT$FCT_SOURCE)
    amountOfForecasters <- length(forcastersList)
    
    # allocate NA covariance
    tempCov <- matrix(nrow = amountOfForecasters, ncol = amountOfForecasters)
    
    # covariance is calculated with trainDT
    # for each element in covariance
    for (i in 1:amountOfForecasters) {
      for (j in 1:amountOfForecasters) {
        
        # get error per forcaster
        ei <- trainDT[FCT_SOURCE == forcastersList[[i]], .(TIME_PERIOD, ERR_VALUE)]
        ej <- trainDT[FCT_SOURCE == forcastersList[[j]], .(TIME_PERIOD, ERR_VALUE)]
        setkey(ei, TIME_PERIOD)
        setkey(ej, TIME_PERIOD)
        
        # get intersect on TIME_PERIOD
        e <- ei[ej, nomatch = 0]
        
        # get covariance
        if (nrow(e) == 0) {
          tempCov[[i, j]] <- 0
        } else {
          tempCov[[i, j]] <- sum(e$ERR_VALUE * e$i.ERR_VALUE) / nrow(e)
        }
      }
    }
    # make sure it is p.d. (choice according to the paper)
    tempCov <- nearPD(tempCov)
    # take the matrix from the output structure of nearPD
    tempCov <- as.data.frame.matrix(tempCov$mat)
    colnames(tempCov) <- forcastersList
    
    # weights is appended to tempDT
    if(isFull){
      # calculate weights using full cov
      tempWeight <- calculateWeight(tempCov)
    }else{
      # get the weights using submatrix
      tempWeight <- calculateWeight(getSubCov(tempCov,tempDT[TIME_PERIOD==last(sort(TIME_PERIOD)),FCT_SOURCE]))
    }
    
    # fill in the weights
    tempWeightDT <- as.data.table(tempWeight, keep.rownames = 'FCT_SOURCE')
    setkey(tempWeightDT, FCT_SOURCE)
    setkey(tempDT, FCT_SOURCE)
    tempDT <- merge(tempDT,tempWeightDT, all.x = TRUE)
    tempDT[is.na(tempWeight), tempWeight := 0]
    return(tempDT$tempWeight)
  }
  
  
  # make a copy of the data table per testing period
  testingList <- seq(2014.25, 2018.25, 0.25)
  testingColumn <- sort(rep(testingList, nrow(DT)))
  DT <- data.table(DT, TEST_PERIOD = testingColumn)
  
  # take only the values up to forecast date to evaluate covariance
  DT <- DT[TIME_PERIOD<=TEST_PERIOD]

  # calculate the weight by running getWeightsSubFunc() per topic, horizon and test periods
  # .SD[TRUE] is because unmodified .SD is locked, so here modify the input by taking all rows with [TRUE]
  print('Full start')
  startPoint <- Sys.time()
  DT[, WEIGHT_FULL := getWeightsSubFunc(isFull = TRUE, .SD[TRUE], .SD[TIME_PERIOD < TEST_PERIOD]), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TEST_PERIOD')]
  print('Full done')
  midPoint <- Sys.time()
  print(midPoint - startPoint)
  DT[, WEIGHT_SUB := getWeightsSubFunc(isFull = FALSE, .SD[TRUE], .SD[TIME_PERIOD < TEST_PERIOD]), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TEST_PERIOD')]
  print('Sub done')
  endPoint <- Sys.time()
  print(endPoint - midPoint)
  print('Total time')
  print(endPoint - startPoint)
  
  # adjust weights full to sum to 1 in every time period
  ## doing this adjustment makes the result worse
  # DT[, WEIGHT_FULL := WEIGHT_FULL/sum(WEIGHT_FULL), by = c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]
  # DT[, WEIGHT_FULL_NONEG := WEIGHT_FULL_NONEG/sum(WEIGHT_FULL_NONEG), by = c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]
  
  # get equal weights
  DT[TIME_PERIOD == TEST_PERIOD, WEIGHT_EQUAL := 1/.N, by = c('FCT_TOPIC','FCT_HORIZON','TEST_PERIOD')]
  # set order by .... to put for each test period, the last obs is the equal weights
  setkey(DT,FCT_TOPIC, FCT_HORIZON, TEST_PERIOD,TIME_PERIOD)
  # take the last obs and append them upwards within test period
  DT[, WEIGHT_EQUAL:= na.locf(WEIGHT_EQUAL, fromLast = T)]
  DT
}