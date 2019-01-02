load(file = 'Input/Initial_Weights.RData')
source('Rcode/global.R', echo=FALSE)

# this script shows the summary statistics of the optimal weight

# calculate the summary statistics
DT_NEG <- DT[WEIGHT_SUB!=0,.(Minimum = round(min(WEIGHT_SUB),2), 
                   First = round(quantile(WEIGHT_SUB,0.25),2), 
                   Mean = round(mean(WEIGHT_SUB),2), 
                   Median = round(quantile(WEIGHT_SUB,0.5),2), 
                   Third = round(quantile(WEIGHT_SUB,0.75),2), 
                   Variance = round(var(WEIGHT_SUB),2), 
                   Maximum = round(max(WEIGHT_SUB),2)), 
   by = c('FCT_TOPIC', 'FCT_HORIZON')]

write.csv(DT_NEG, file='Output/negWeightsSummary.csv', row.names = F)
