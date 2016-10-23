# install.packages('data.table')
# install.packages('dplyr')
# install.packages('tvm')
# install.packages('zoo')

parseDate = function (val) { as.Date(val, format='%d.%m.%Y') }
parseFloat = function (val) { as.numeric(sub(',', '.', val)) }
parseDoubleQuotesDates = function (val) {as.Date(substr(val,3,12), format = "%d/%m/%Y")}
parseDoubleQuotesNumeric = function (val) {
    lengthOfVal = nchar(val)
    as.numeric(substr(val,3,lengthOfVal - 2))
}


pensioniKeskusFileEncoding = 'UCS-2LE'
readFile = function (fileName) {
	read.table(fileName, sep = '	', quote = '',
						header = TRUE, fileEncoding = pensioniKeskusFileEncoding)
}
mergeData = function (assets, nav) {

	message("Message: mergeData is depracetd. Use calcStats() and generateDb() istead.")

	if (nrow(assets) != nrow(nav)) {
		stop('nrow(assets) != nrow(nav)')
	}
	if (!all(assets$Kuupäev == nav$Kuupäev)) {
		stop('!all(assets$Kuupäev == nav$Kuupäev)')
	}
	d   = data.frame(time=parseDate(nav$Kuupäev), nav=parseFloat(nav$NAV), volume=assets$Maht)
	d$q = d$volume/d$nav

	# Eeldame, et kp järgi sorteeritud
	d$changeOfQ = d$q - c(0, d$q[1:(length(d$q)-1)])

	d$cf = -d$changeOfQ*d$nav
	# head(d$changeOfQ - d$q)
	d$c  = d$volume / d$volume[1]
	return(d)
}

yearfrac = function(start, ends) {
	as.numeric(difftime(start, ends, units = 'days')) / 365
}

getStats = function (transactions, fee) {
    # transactions - ühe fondi andmed ???
	L = nrow(transactions)
	last = transactions[L,]
	last$cf = last$volume
	d = rbind(transactions, last)
	# r - tootlus
	r = tvm::xirr(d$cf, d$time, interval=c(0,10), tol=.Machine$double.eps^0.5)
	d$yrf = yearfrac(max(d$time), d$time)
	d$m = d$cf*(1+r+fee)
	d$pvgross = (-d$m)**(d$yrf)
	pv=tvm::xnpv(i = r+fee, cf = -transactions$cf, d = transactions$time)
	totalFee = (pv*(1+r+fee)**max(d$yrf))-last$volume
	totalProfit = last$volume-sum(-transactions$cf)
	totalCf=sum(-transactions$cf)
	return(list(
		r=r,
		pv=pv,
		profitPerCf1000=1000*totalProfit/totalCf,
		feePerCf1000=1000*totalFee/totalCf,
		startDate=min(transactions$time),
		endDate=max(transactions$time),
		totalCf=totalCf,
		totalProfit=totalProfit,
		totalFee=totalFee
	))
}


getStatsTimeWindow = function(transactions, fee, 
                              start = "2000-01-01", end = "2017-01-01"){
    
    # Args
    #  transactions :
    #  fee          :
    #  start        : excpecting string formated as "2016-08-25"
    #  end          : excpecting string formated as "2016-08-25"
    
    start = as.Date(start)
    end   = as.Date(end)
    subTransactions = dplyr::filter(transactions, time >= start & time <= end)
    # !TODO - if timewindow sucks, recommend better one
    
    # recalculating c, changeOfQ, cf
    subTransactions = dplyr::rename(subTransactions, ISIN = isin, Fond = fond)
    subTransactions = calcStats(subTransactions)
    
    getStats(transactions = subTransactions, fee = fee)
}


# Example:
# getComparisonIndexFond(db$EE3600019782)
# getComparisonIndexFond(db$EE3600019782, indexRatio = 0.25, indb = indb)
getComparisonIndexFond = function (fond, indexRatio=0.5, indb=getIndexFundsData(path = '/raw-data')) {
	if (indexRatio < 0 | indexRatio > 1) {
		stop('indexRatio not in [0, 1]')
	}
	pen = fond
	bnd = indb$IE0009591805
	idx = indb$IE00B03HCZ61

	mrg = merge(dplyr::select(pen, -nav, -c, -volume), dplyr::select(idx, time, idxPrice=nav), by = 'time', all.x = TRUE)
	mrg = merge(mrg, dplyr::select(bnd, time, bndPrice=nav), by='time', all.x = TRUE)

	percentIndexRatio = round(100 * indexRatio)
	mrg$fond = paste0(mrg$fond, ' vs ', percentIndexRatio, '/', 100 - percentIndexRatio)

	mrg$idxPrice = zoo::na.locf(mrg$idxPrice)
	mrg$idxChangeOfQuantity = -mrg$cf * indexRatio / mrg$idxPrice
	mrg$idxQuantity = cumsum(mrg$idxChangeOfQuantity)
	mrg$idxVolume = mrg$idxPrice * mrg$idxQuantity

	mrg$bndPrice = zoo::na.locf(mrg$bndPrice)
	mrg$bndChangeOfQuantity = -mrg$cf * (1-indexRatio) / mrg$bndPrice
	mrg$bndQuantity = cumsum(mrg$bndChangeOfQuantity)
	mrg$bndVolume = mrg$bndPrice * mrg$bndQuantity

	mrg$volume = mrg$idxVolume + mrg$bndVolume

	mrg
}
