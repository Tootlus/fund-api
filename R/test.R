


source("R/get-clean-data.R")
source("R/utils.r")
test = getIndexFundsData(path = "/raw-data"); head(test)
test = generateDb(path = "/raw-data")
