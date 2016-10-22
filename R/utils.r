# install.packages(data.table)
# install.packages(dplyr)
# install.packages(tvm)

parseDate = function (val) { as.Date(val, format='%d.%m.%Y') }
parseFloat = function (val) { as.numeric(sub(',', '.', val)) }
parseDoubleQuotesDates = function (val) {as.Date(substr(val,3,12), format = "%d/%m/%Y")}
parseDoubleQuotesNumeric = function (val) {
    lengthOfVal = nchar(val)[1]
    as.numeric(substr(val,3,lengthOfVal - 2))}


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

calcStats = function (d) {
	# arrange by date
	d <- dplyr::arrange(d, time)
	d$q = d$volume / d$nav

	# Eeldame, et kp järgi sorteeritud
	d$changeOfQ = d$q - c(0, d$q[1:(length(d$q)-1)])

	d$cf = -d$changeOfQ*d$nav
	# head(d$changeOfQ - d$q)
	d$c  = d$volume / d$volume[1]

	d = dplyr::select(d, isin = ISIN, fond = Fond, time, nav, volume,
										quantity = q, changeOfQuantity = changeOfQ, cf, c)
	d
}

yearfrac = function(start, ends) {
	as.numeric(difftime(start, ends, units = 'days')) / 365
}

getStats = function (transactions, fee) {
	L = nrow(transactions)
	last = transactions[L,]
	last$cf = last$volume
	d = rbind(transactions, last)
	r = tvm::xirr(d$cf, d$time, interval=c(0,10), tol=.Machine$double.eps^0.5)
	d$yrf = yearfrac(max(d$time), d$time)
	d$m = d$cf*(1+r+fee)
	d$pvgross = (-d$m)**(d$yrf)
	pv=tvm::xnpv(i = r+fee, cf = -transactions$cf, d = transactions$time)
	totalfee = (pv*(1+r+fee)**max(d$yrf))-last$volume
	profit = last$volume-sum(-transactions$cf)
	return(list(
		r=r,
		pv=pv,
		profit=profit,
		totalfee=totalfee
	))
}
