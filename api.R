
source("R/get-clean-data.R")
source("R/utils.r")

#* @get /generateDb
api.generateDb = function(path = "/raw-data"){
    generateDb(path = path)
}

#* @get /getIndexFundsData
api.getIndexFundsData = function(path = "/raw-data"){
    getIndexFundsData(path = path)
}

#* @get /testPlumber
api.testPlumber = function(a,b){
    testPlumber(a,b)
}
