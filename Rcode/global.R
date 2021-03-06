# load packages
if (!require('data.table')) install.packages('data.table'); library(data.table)
if (!require('Matrix')) install.packages('Matrix'); library(Matrix)
if (!require('zoo')) install.packages('zoo'); library(zoo)
if (!require('stringr')) install.packages('stringr'); library(stringr)
if (!require('MASS')) install.packages('MASS'); library(MASS)

# load functions
source('Rcode/getWeights.R', echo=FALSE)
source('Rcode/getForecast.R', echo=FALSE)
source('Rcode/getNewWeights.R', echo=FALSE)
source('Rcode/getEval.R', echo=FALSE)
source('Rcode/getForecast_truncate.R', echo=FALSE)
source('Rcode/getNewWeights_truncate.R', echo=FALSE)
source('Rcode/getEval_truncate.R', echo=FALSE)
source('Rcode/getEval_insample.R', echo=FALSE)