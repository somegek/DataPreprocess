# this script loads the preprocessed data and make the summary statistics 
# for true value within model space or ture value outside of model space
# table 1

load(file = "Input/preprocessed.RData")
source('Rcode/global.R', echo=F)

# get the minimum and maximum of the individual forecasts
DT_MS <- unique(DT[, .(TRUE_VALUE, MIN_VALUE = min(OBS_VALUE), MAX_VALUE = max(OBS_VALUE)), by = c('FCT_TOPIC', 'FCT_HORIZON', 'TIME_PERIOD')])

# if outside, Outside = 1, else Outside = 0
DT_MS[TRUE_VALUE<MIN_VALUE | TRUE_VALUE>MAX_VALUE, Outside := 1]
DT_MS[TRUE_VALUE>=MIN_VALUE & TRUE_VALUE<=MAX_VALUE, Outside := 0]

write.csv(DT_MS, file='Output/ModelSpaceInsideOrOutside.csv',row.names = F)

# make a summary on the model space
mdlSpaceSummary <- DT_MS[,.(Minimum = min(Outside), 
                            First = quantile(Outside,0.25), 
                            Mean = round(mean(Outside),2), 
                            Median = quantile(Outside,0.5), 
                            Third = quantile(Outside,0.75), 
                            IQR = IQR(Outside), 
                            Maximum = max(Outside)), 
                         by = c('FCT_TOPIC', 'FCT_HORIZON')]
write.csv(mdlSpaceSummary, file='Output/ModelSpaceSummary.csv',row.names = F)
