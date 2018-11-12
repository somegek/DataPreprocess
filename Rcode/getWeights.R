getWeights <- function(DT) {

  # get the sub covariance matrix given name list
  getSubCov <- function(Cov, colNameList) {
    colInd <- which(colnames(Cov) %in% colNameList)
    Cov[colInd, colInd]
  }

  # get the weights using simlpe formula
  calculateWeight <- function(Cov) {
    weight <- solve(Cov, rep(1, ncol(Cov)))
    weight <- weight / sum(weight)
    weight
  }

  getBias <- function(tempDT, trainDT) {
    # calculate demeaned error and forecast
    colNamesList <- c("OBS_DEMEAN", "ERR_DEMEAN")
    colSelectList <- c("OBS_VALUE", "ERR_VALUE")
    trainDT[, (colNamesList) := lapply(.SD, function(x) x - mean(x)), .SDcols = colSelectList, by = c("FCT_SOURCE")]

    # use ERR_DEMEAN ~ OBS_DEMEAN - 1 to calculate beta per forecaster
    trainDT[, ERR_B := lm(ERR_DEMEAN ~ OBS_DEMEAN - 1, data = .SD[TRUE])$coefficients, by = c("FCT_SOURCE")]
    # calculate alpha over different forecaster
    trainDT[, ERR_A := mean(ERR_VALUE - ERR_B * OBS_VALUE)]
    trainDT[, BIAS := ERR_A + ERR_B * OBS_VALUE]

    # combine with test data
    tempDT <- merge(tempDT, unique(trainDT[, .(FCT_SOURCE, ERR_B, ERR_A)]), by = c("FCT_SOURCE"))

    # forecast ERR_VALUE
    tempDT[, BIAS := ERR_A + ERR_B * OBS_VALUE]

    # change the ERR_VALUE in trainDT for covariance calculation
    trainDT[, ERR_VALUE := ERR_VALUE - BIAS]
    tempCov <- getCovariance(tempDT, trainDT)

    list(bias = tempDT[TIME_PERIOD == last(sort(TIME_PERIOD)), BIAS], Cov = tempCov)
  }

  # get bias weights
  getBiasWeights <- function(tempDT, trainDT) {
    biasList <- getBias(tempDT, trainDT)
    Cov_Err <- getSubCov(biasList$Cov, tempDT[TIME_PERIOD == last(sort(TIME_PERIOD)), FCT_SOURCE]) + biasList$bias %o% biasList$bias
    weight <- calculateWeight(Cov_Err)
    weight
  }

  getCovariance <- function(tempDT, trainDT) {
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
    tempCov
  }


  # get the weights by first getting the covariance, then calculate the weights
  getWeightsSubFunc <- function(isFull, tempDT, trainDT) {

    # weights is appended to tempDT
    if (isFull) {
      # calculate weights using full cov
      tempWeight <- getBiasWeights(tempDT, trainDT)
    } else {
      tempCov <- getCovariance(tempDT, trainDT)
      # get the weights using submatrix
      tempWeight <- calculateWeight(getSubCov(tempCov, tempDT[TIME_PERIOD == last(sort(TIME_PERIOD)), FCT_SOURCE]))
    }

    # fill in the weights
    tempWeightDT <- as.data.table(tempWeight, keep.rownames = "FCT_SOURCE")
    setkey(tempWeightDT, FCT_SOURCE)
    setkey(tempDT, FCT_SOURCE)
    tempDT <- merge(tempDT, tempWeightDT, all.x = TRUE)
    tempDT[is.na(tempWeight), tempWeight := 0]
    return(tempDT$tempWeight)
  }


  # make a copy of the data table per testing period
  testingList <- seq(2014.25, 2018.25, 0.25)
  testingColumn <- sort(rep(testingList, nrow(DT)))
  DT <- data.table(DT, TEST_PERIOD = testingColumn)

  # take only the values up to forecast date to evaluate covariance
  DT <- DT[TIME_PERIOD <= TEST_PERIOD]

  # calculate the weight by running getWeightsSubFunc() per topic, horizon and test periods
  # .SD[TRUE] is because unmodified .SD is locked, so here modify the input by taking all rows with [TRUE]
  print("Full start")
  startPoint <- Sys.time()
  DT[, WEIGHT_FULL := getWeightsSubFunc(isFull = TRUE, .SD[TRUE], .SD[TIME_PERIOD < TEST_PERIOD]), by = c("FCT_TOPIC", "FCT_HORIZON", "TEST_PERIOD")]
  print("Full done")
  midPoint <- Sys.time()
  print(midPoint - startPoint)
  DT[, WEIGHT_SUB := getWeightsSubFunc(isFull = FALSE, .SD[TRUE], .SD[TIME_PERIOD < TEST_PERIOD]), by = c("FCT_TOPIC", "FCT_HORIZON", "TEST_PERIOD")]
  print("Sub done")
  endPoint <- Sys.time()
  print(endPoint - midPoint)
  print("Total time")
  print(endPoint - startPoint)

  # get equal weights
  DT[TIME_PERIOD == TEST_PERIOD, WEIGHT_EQUAL := 1 / .N, by = c("FCT_TOPIC", "FCT_HORIZON", "TEST_PERIOD")]
  # set order by .... to put for each test period, the last obs is the equal weights
  setkey(DT, FCT_TOPIC, FCT_HORIZON, TEST_PERIOD, TIME_PERIOD)
  # take the last obs and append them upwards within test period
  DT[, WEIGHT_EQUAL := na.locf(WEIGHT_EQUAL, fromLast = T)]
  DT
}