# ECB page
# http://sdw.ecb.europa.eu/browse.do?node=9691152
# direct link (Bulk download)
# https://sdw-wsrest.ecb.europa.eu/service/data/SPF?format=csvdata
# data.csv is renamed to SPF.csv
# no further modification
DT <- fread("Input/SPF.csv")


######### remove excess columns and rows #########
# > colnames(DT)
# [1] "KEY"           "FREQ"          "REF_AREA"      "FCT_TOPIC"     "FCT_BREAKDOWN" "FCT_HORIZON"
# [7] "SURVEY_FREQ"   "FCT_SOURCE"    "TIME_PERIOD"   "OBS_VALUE"     "OBS_STATUS"    "OBS_CONF"
# [13] "OBS_PRE_BREAK" "OBS_COM"       "TIME_FORMAT"   "COLLECTION"    "COMPILING_ORG" "DISS_ORG"
# [19] "DECIMALS"      "SOURCE_AGENCY" "TITLE"         "TITLE_COMPL"   "UNIT"          "UNIT_MULT"

# select columns
DT <- subset(DT, select = c(
  "FREQ",
  "FCT_TOPIC",
  "FCT_BREAKDOWN",
  "FCT_HORIZON",
  "FCT_SOURCE",
  "TIME_PERIOD",
  "OBS_VALUE"
))

# remove rows that are not required
DT <- DT[FCT_TOPIC != "ASSU" &
  FCT_BREAKDOWN == "POINT" &
  !FCT_SOURCE %in% c("AVG", "NUM", "VAR", "ECB") &
  !is.na(OBS_VALUE) &
  FREQ != "A"]

# remove now redundant columns
DT[, FCT_BREAKDOWN := NULL]
DT[, FREQ := NULL]

#### DT after cleaning up
#        FCT_TOPIC FCT_HORIZON FCT_SOURCE TIME_PERIOD OBS_VALUE
#     1:      HICP        P12M        001        1999      1.00
#     2:      HICP        P12M        001        2000      1.80
#     3:      HICP        P12M        001        2001      1.90
#     4:      HICP        P12M        001        2002      1.80
#     5:      HICP        P12M        001        2003      2.00
# ---
# 63913:      RGDP         P9M        116     2018-Q4      2.50
# 63914:      RGDP         P9M        118     2018-Q1      1.70
# 63915:      RGDP         P9M        118     2018-Q2      1.70
# 63916:      RGDP         P9M        118     2018-Q4      1.87
# 63917:      RGDP         P9M        119     2018-Q1      1.70


################ data quality correction ###############
#### the paper said that 1 year and 2 year is most data rich, so judging on the data it is
#### 12M and 24M for HICP and UNEM, and 9M and 21M for RGDP

# > table(DT[FCT_TOPIC=='HICP',FCT_HORIZON])
# P12M P24M P60M
# 3932 3497  130
#
# > table(DT[FCT_TOPIC=='RGDP',FCT_HORIZON])
# P21M P57M P60M  P9M
# 3505   70   53 3867
#
# > table(DT[FCT_TOPIC=='UNEM',FCT_HORIZON])
# P12M P24M P60M
# 3556 3183  115

DT_HICP_UNEM <- DT[FCT_TOPIC %in% c("HICP", "UNEM") & FCT_HORIZON %in% c("P12M", "P24M"), ]
DT_RGDP <- DT[FCT_TOPIC == "RGDP" & FCT_HORIZON %in% c("P9M", "P21M"), ]
DT <- rbind(DT_HICP_UNEM, DT_RGDP)
rm(DT_HICP_UNEM, DT_RGDP)
# make all 12 or 24
DT[FCT_HORIZON == "P9M", FCT_HORIZON := "P12M"]
DT[FCT_HORIZON == "P21M", FCT_HORIZON := "P24M"]
# make all 1 or 2
DT[FCT_HORIZON == "P12M", FCT_HORIZON := "1"]
DT[FCT_HORIZON == "P24M", FCT_HORIZON := "2"]
DT[, FCT_HORIZON := as.numeric(FCT_HORIZON)]

## data correction
# 2 observations of UNEM has December instead of november
DT[FCT_TOPIC == "UNEM" & TIME_PERIOD == "2000-12", TIME_PERIOD := "2000-11" ]
DT[FCT_TOPIC == "UNEM" & TIME_PERIOD == "2001-12", TIME_PERIOD := "2001-11" ]

# transformation to 'YYYY QQ' for consistency purpose
DT[FCT_TOPIC == "RGDP", TIME_PERIOD := gsub("-", "", TIME_PERIOD)]
DT[FCT_TOPIC == "HICP", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q", as.numeric(substr(TIME_PERIOD, 6, 7)) / 3)]
DT[FCT_TOPIC == "UNEM", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q", (as.numeric(substr(TIME_PERIOD, 6, 7)) + 1) / 3)]
DT[, TIME_PERIOD := as.yearqtr(TIME_PERIOD)]

setkey(DT, FCT_TOPIC, FCT_HORIZON, FCT_SOURCE, TIME_PERIOD)

save(DT, file = "Input/SPF_pre_filter.RData")








############### Get true value for Macro varaibles ###############
# RGDP key
# MNA.Q.N.U2.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N
# https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=320.MNA.Q.N.U2.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N&start=&end=&submitOptions.x=0&submitOptions.y=0&trans=YPC
# removed the first 4 rows in csv

RGDP <- fread("Input/RGDP.csv")

# remove level column
RGDP[, `[Millions]` := NULL]

# give matching column name
colnames(RGDP) <- c("TIME_PERIOD", "TRUE_VALUE")
RGDP[, TIME_PERIOD := as.yearqtr(TIME_PERIOD)]

# add data identifier
RGDP[, TRUE_TOPIC := "RGDP"]
RGDP <- RGDP[TIME_PERIOD >= "1999 Q3"]
setkey(RGDP, TIME_PERIOD)

# UNEM key
# https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=132.STS.M.U2.S.UNEH.RTT000.4.000&start=&end=&submitOptions.x=0&submitOptions.y=0&trans=N
# removed the first 4 rows in csv

UNEM <- fread("Input/UNEM.csv")

# give matching column name
colnames(UNEM) <- c("TIME_PERIOD", "TRUE_VALUE")

# selection correct month
UNEM[substr(TIME_PERIOD, 5, 7) == "Feb", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q1")]
UNEM[substr(TIME_PERIOD, 5, 7) == "May", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q2")]
UNEM[substr(TIME_PERIOD, 5, 7) == "Aug", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q3")]
UNEM[substr(TIME_PERIOD, 5, 7) == "Nov", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q4")]
# TIME_PERIOD is now either YYYYMMM or YYYYQQ, take only YYYYQQ
UNEM <- UNEM[nchar(TIME_PERIOD) == 6]

# time transformation
UNEM[, TIME_PERIOD := as.yearqtr(TIME_PERIOD)]
UNEM <- UNEM[TIME_PERIOD >= "1999 Q3" & TIME_PERIOD <= "2018 Q2"]

# add data identifier
UNEM[, TRUE_TOPIC := "UNEM"]
setkey(UNEM, TIME_PERIOD)

# HICP key
# https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=122.ICP.M.U2.N.000000.3.ANR
# removed the first 4 rows in csv

HICP <- fread("Input/HICP.csv")

# give matching column name
colnames(HICP) <- c("TIME_PERIOD", "TRUE_VALUE")

# selection correct month
HICP[substr(TIME_PERIOD, 5, 7) == "Mar", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q1")]
HICP[substr(TIME_PERIOD, 5, 7) == "Jun", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q2")]
HICP[substr(TIME_PERIOD, 5, 7) == "Sep", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q3")]
HICP[substr(TIME_PERIOD, 5, 7) == "Dec", TIME_PERIOD := paste0(substr(TIME_PERIOD, 1, 4), "Q4")]
# TIME_PERIOD is now either YYYYMMM or YYYYQQ, take only YYYYQQ
HICP <- HICP[nchar(TIME_PERIOD) == 6]

# time transformation
HICP[, TIME_PERIOD := as.yearqtr(TIME_PERIOD)]
HICP <- HICP[TIME_PERIOD >= "1999 Q3" & TIME_PERIOD <= "2018 Q2"]

# add data identifier
HICP[, TRUE_TOPIC := "HICP"]
setkey(HICP, TIME_PERIOD)

# combine true data
MacroData <- rbind(HICP, UNEM, RGDP)
MacroData[, TRUE_VALUE := as.numeric(TRUE_VALUE)]

save(RGDP, UNEM, HICP, MacroData, file = "Input/MacroData.RData")

# add true value to forecast data table
for (topic in c("HICP", "UNEM", "RGDP")) {
  for (time in unique(MacroData$TIME_PERIOD)) {
    DT[FCT_TOPIC == topic & TIME_PERIOD == time, TRUE_VALUE := MacroData[TRUE_TOPIC == topic & TIME_PERIOD == time, TRUE_VALUE]]
  }
}
# remove future values where forecasts exists
DT <- DT[!is.na(TRUE_VALUE)]

rm(MacroData, HICP, UNEM, RGDP)

############ Data Filter ###################

# filter on amount
DT[, N := .N, by = c("FCT_TOPIC", "FCT_HORIZON", "FCT_SOURCE")]
DT <- DT[N >= 24]
# DT[,.(TOTAL_FCT_SOURCE = length(unique(FCT_SOURCE))),by=c('FCT_TOPIC', 'FCT_HORIZON')]
#    FCT_TOPIC FCT_HORIZON TOTAL_FCT_SOURCE
# 1:      HICP           1               72
# 2:      HICP           2               60
# 3:      RGDP           1               70
# 4:      RGDP           2               64
# 5:      UNEM           1               65
# 6:      UNEM           2               53
# remove excess column
DT[,N:=NULL]

############ Error Calculation ###################
# add error value
DT[, ERR_VALUE := TRUE_VALUE - OBS_VALUE]


write.csv(DT, file = "Input/SPF_Final.csv")
save(DT, file = "Input/preprocessed.RData")

rm(time,topic)
