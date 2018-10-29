load(file = 'Input/Initial_Weights.RData')
source('Rcode/global.R', echo=FALSE)

DT_NEG <- DT[WEIGHT_SUB!=0,.(Minimum = round(min(WEIGHT_SUB),2), 
                   First = round(quantile(WEIGHT_SUB,0.25),2), 
                   Mean = round(mean(WEIGHT_SUB),2), 
                   Median = round(quantile(WEIGHT_SUB,0.5),2), 
                   Third = round(quantile(WEIGHT_SUB,0.75),2), 
                   IQR = round(IQR(WEIGHT_SUB),2), 
                   Maximum = round(max(WEIGHT_SUB),2)), 
   by = c('FCT_TOPIC', 'FCT_HORIZON')]

write.csv(DT_NEG, file='Output/negWeightsSummary.csv', row.names = F)
