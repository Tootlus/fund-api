source('R/get-clean-data.R')
source('R/utils.r')

db = generateDb('/raw-data')
indb = getIndexFundsData(path = '/raw-data')

#* @get /getStats
api.getStats = function (isin, fee=0.016) {
	d = db[[isin]]

	if (is.null(isin)) {
		return('No such isin.')
	}

	getStats(d, as.numeric(fee))
}

#* @get /getComparison
api.getComparison = function (isin, indexRatio=0.5, fee=0.016) {
	d = db[[isin]]

	if (is.null(isin)) {
		return('No such isin.')
	}

	getStats(getComparisonIndexFond(d, indexRatio=as.numeric(indexRatio), indb=indb), fee)
}

#* @get /testPlumber
api.testPlumber = function(a = 1, b = 2){
    testPlumber(a,b)
}
