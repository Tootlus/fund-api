


source("R/get-clean-data.R")
source("R/utils.r")
test = getIndexFundsData(path = "/raw-data"); head(test)
test = generateDb(path = "/raw-data")

library(plumber)
r = plumb("R/get-clean-data.R")  # Where 'myfile.R' is the location of the file shown above
r$run(port=8000)