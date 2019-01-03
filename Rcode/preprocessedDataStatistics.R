source('Rcode/global.R', echo=FALSE)
#load preprocessed data
load(file = "Input/preprocessed.RData")

# calculate amount per time and per forecast
DT[,AMOUNT_PER_TIME:=.N,by=c('FCT_TOPIC','FCT_HORIZON','TIME_PERIOD')]
DT[,AMOUNT_PER_FORECASTERS:=.N,by=c('FCT_TOPIC','FCT_HORIZON','FCT_SOURCE')]


DT_FCT <- DT[,.(SUMMARY=summary(AMOUNT_PER_FORECASTERS)), by = c('FCT_TOPIC','FCT_HORIZON')]
# after dcast stats is ordered, so first give it numeric identifier to avoid row reordering
DT_FCT[,STATS:=1:6]
# change long table to wide table
DT_FCT[,FCT_TOPIC_HORZ:=paste0(FCT_TOPIC,FCT_HORIZON)]
DT_FCT[,FCT_TOPIC:=NULL]
DT_FCT[,FCT_HORIZON:=NULL]
DT_FCT <- dcast(DT_FCT, STATS~FCT_TOPIC_HORZ, value.var = 'SUMMARY')
DT_FCT[,STATS:=c('Minimum','First Q','Median','Mean','Third Q','Maximum')]
#     STATS HICP1 HICP2 RGDP1 RGDP2 UNEM1 UNEM2
# 1:    Min 24.00  24.0 26.00 24.00 25.00 24.00
# 2:  1st Q 40.00  40.0 42.00 35.00 40.00 36.00
# 3: Median 55.00  52.0 55.00 51.00 53.00 52.00
# 4:   Mean 52.85  49.6 53.64 49.16 51.85 49.65
# 5:  3rd Q 67.00  62.0 66.00 63.00 66.00 63.00
# 6:    Max 74.00  70.0 75.00 71.00 74.00 70.00

DT_FCT[,(c('HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2')):=lapply(.SD,round),.SDcols=c('HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2')]
write.csv(DT_FCT,file='Output/FctSummary.csv',row.names = FALSE)


DT_TIME <- DT[,.(SUMMARY=summary(AMOUNT_PER_TIME)), by = c('FCT_TOPIC','FCT_HORIZON')]
DT_TIME[,STATS:=1:6]
# change long table to wide table
DT_TIME[,FCT_TOPIC_HORZ:=paste0(FCT_TOPIC,FCT_HORIZON)]
DT_TIME[,FCT_TOPIC:=NULL]
DT_TIME[,FCT_HORIZON:=NULL]
DT_TIME <- dcast(DT_TIME, STATS~FCT_TOPIC_HORZ, value.var = 'SUMMARY')
DT_TIME[,STATS:=c('Minimum','First Q','Median','Mean','Third Q','Maximum')]
#     STATS HICP1 HICP2 RGDP1 RGDP2 UNEM1 UNEM2
# 1:    Min 37.00 26.00 38.00 28.00 32.00 22.00
# 2:  1st Q 43.00 35.00 42.00 36.00 38.00 30.00
# 3: Median 45.00 38.00 45.00 40.00 40.00 34.00
# 4:   Mean 46.09 38.57 45.12 40.06 40.99 34.19
# 5:  3rd Q 49.00 43.00 48.00 45.00 44.00 38.00
# 6:    Max 54.00 49.00 54.00 52.00 51.00 44.00

DT_TIME[,(c('HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2')):=lapply(.SD,round),.SDcols=c('HICP1', 'HICP2', 'RGDP1', 'RGDP2', 'UNEM1', 'UNEM2')]
write.csv(DT_TIME,file='Output/ObsSummary.csv',row.names = FALSE)

# get amount per time for the forecast period
DT_TEST <- unique(DT[TIME_PERIOD>=2016,.(FCT_TOPIC,FCT_HORIZON,TIME_PERIOD,AMOUNT_PER_TIME)])
setkey(DT_TEST,FCT_TOPIC,FCT_HORIZON,TIME_PERIOD)

# change long table to wide table
# merge topic and horizon
DT_TEST[,FCT_TOPIC_HORZ:=paste0(FCT_TOPIC,FCT_HORIZON)]
DT_TEST[,FCT_TOPIC:=NULL]
DT_TEST[,FCT_HORIZON:=NULL]
# change to wide
DT_WIDE <- dcast(DT_TEST, TIME_PERIOD ~ FCT_TOPIC_HORZ, value.var = 'AMOUNT_PER_TIME')
#    TIME_PERIOD HICP1 HICP2 RGDP1 RGDP2 UNEM1 UNEM2
# 1:     2016 Q1    45    32    38    33    41    30
# 2:     2016 Q2    40    33    42    37    36    28
# 3:     2016 Q3    44    35    42    38    39    28
# 4:     2016 Q4    45    34    41    40    39    30
# 5:     2017 Q1    39    35    38    30    32    32
# 6:     2017 Q2    38    27    38    35    33    25
# 7:     2017 Q3    37    31    43    36    33    29
# 8:     2017 Q4    43    34    41    36    36    29
# 9:     2018 Q1    40    27    46    30    34    22
# 10:    2018 Q2    44    26    45    31    40    23
write.csv(DT_WIDE,file='Output/ObsPerTestPeriod.csv',row.names = FALSE)
