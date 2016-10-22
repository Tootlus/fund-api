
# install.packages(data.table)
# install.packages(dplyr)

parseDate = function (val) { as.Date(val, format='%d.%m.%Y') }
parseFloat = function (val) { as.numeric(sub(',', '.', val)) }
pensioniKeskusFileEncoding = 'UCS-2LE'
readFile = function (fileName) { read.table(fileName, sep = '	', quote = '', 
                                            header = TRUE, fileEncoding = pensioniKeskusFileEncoding) }
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

calcStats <- function(d){
    # arrange by date
    d <- dplyr::arrange(d, time)
    
    # 
    d$q = d$volume/d$nav
    
    # Eeldame, et kp järgi sorteeritud
    d$changeOfQ = d$q - c(0, d$q[1:(length(d$q)-1)])
    
    d$cf = -d$changeOfQ*d$nav
    # head(d$changeOfQ - d$q)
    d$c  = d$volume / d$volume[1]
    
    d = dplyr::select(d,isin = ISIN, fond = Fond, time, nav, volume, quantity = q, 
                      changeOfQuantity = changeOfQ, cf, c)
    d
}


