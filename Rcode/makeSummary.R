makeSummary <- function(DT,x, by){
  browser()
  DT[,.(Minimum = min(x), First = quantile(x,0.25), Mean = mean(x), Median = quantile(x,0.5), Third = quantile(x,0.75), IQR = IQR(x), Maximum = max(x)), by = by, with = TRUE]
  
}