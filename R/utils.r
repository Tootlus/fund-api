
parseDate = function (val) { as.Date(val, format='%d.%m.%Y') }
parseFloat = function (val) { as.numeric(sub(',', '.', val)) }
pensioniKeskusFileEncoding = 'UCS-2LE'
readFile = function (fileName) { read.table(fileName, sep = '	', quote = '', header = TRUE, fileEncoding = pensioniKeskusFileEncoding) }
mergeData = function (assets, nav) {
	if (nrow(assets) != nrow(nav)) {
		stop('nrow(assets) != nrow(nav)')
	}
	if (!all(assets$Kuupäev == nav$Kuupäev)) {
		stop('!all(assets$Kuupäev == nav$Kuupäev)')
	}
	d=data.frame(time=parseDate(nav$Kuupäev), nav=parseFloat(nav$NAV), volume=assets$Maht)
	d$q=d$volume/d$nav
	d$changeOfQ = d$q - c(0, d$q[1:(length(d$q)-1)])
	d$cf=-d$changeOfQ*d$nav
	head(d$changeOfQ - d$q)
	d$c=d$volume / d$volume[1]
	return(d)
}
