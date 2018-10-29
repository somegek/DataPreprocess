load(file = "Input/preprocessed.RData")
source('~/Github/DataPreprocess/Rcode/global.R', echo=F)

DT_MS <- unique(DT[, .(TRUE_VALUE, MIN_VALUE = min(OBS_VALUE), MAX_VALUE = max(OBS_VALUE)), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TIME_PERIOD')])

DT_MS[TRUE_VALUE<MIN_VALUE | TRUE_VALUE>MAX_VALUE, Outside := 1]
DT_MS[TRUE_VALUE>=MIN_VALUE & TRUE_VALUE<=MAX_VALUE, Outside := 0]

write.csv(DT_MS, file='Output/ModelSpaceInsideOrOutside.csv')
mdlSpaceSummary <- DT_MS[,.(Minimum = min(Outside), 
                            First = quantile(Outside,0.25), 
                            Mean = round(mean(Outside),2), 
                            Median = quantile(Outside,0.5), 
                            Third = quantile(Outside,0.75), 
                            IQR = IQR(Outside), 
                            Maximum = max(Outside)), 
                         by = c('FCT_TOPIC', 'FCT_HORIZON')]
write.csv(mdlSpaceSummary, file='Output/ModelSpaceSummary.csv')
