source('R/get-clean-data.R')
source('R/utils.r')

db = generateDb('/raw-data')
indb = getIndexFundsData(path = '/raw-data')

#* @get /getStats
api.getStats = function (isin, fee=0.016) {
	d = db[[isin]]

	message('Stats for ', isin);

	if (is.null(d)) {
		return(list(
			message='Unknown isin.',
			success=FALSE
		))
	}

	getStats(d, as.numeric(fee))
}

#* @get /getComparison
api.getComparison = function (isin, indexRatio=0.5, fee=0.016) {
	d = db[[isin]]

	message('Comparison between ', isin, ' and ', indexRatio)

	if (is.null(d)) {
		return(list(
			message='Unknown isin.',
			success=FALSE
		))
	}

	getStats(getComparisonIndexFond(d, indexRatio=as.numeric(indexRatio), indb=indb), fee)
}
