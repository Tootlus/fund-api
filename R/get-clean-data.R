# lapply(fonds, head)

source('R/utils.r')

readDataFiles = function (dataDir) {
    files  = list.files(dataDir, full.names = T)

    contents = list()
    for (i in files) {
        message('reading ', i)
        contents[[i]] = readFile(i)
    }

    data.table::rbindlist(contents)
}

generateDb = function (path = NULL) {
    # Function for generating database from raw data files
    #
    # Args:
    #  path :  path to raw data folder

    #
    message('Message: working directory is expected to contain folder /raw-data. Parameter "path" should lead to /raw-data')

    wd = getwd()
    path = paste0(wd,path)
    if(!all(c('nav','vol') %in% list.files(path))) stop('"path" must direct to /raw-data
                                                         folder with subdirectories "nav" and "vol"')

    nav = readDataFiles(paste0(path,'/nav'))
    maht = readDataFiles(paste0(path,'/vol'))

    # merged data
    df = merge(nav, maht, by = c('Kuupäev', 'Fond', 'Lühinimi', 'ISIN'))

    df$time   = parseDate(df$Kuupäev)
    df$nav    = parseFloat(df$NAV)
    df$volume = df$Maht

    df = df[!df$volume == 0 & (!is.na(df$volume)),]

    # make list of fonds
    fonds = list()
    for(i in levels(df$ISIN)) fonds[[i]] = dplyr::filter(df, ISIN == i)

    fonds = lapply(fonds, calcStats)
    message('Message: deleted rows where volume == 0')


    fonds
}

getIndexFundsData = function (path = NULL) {
    message('Message: index fund raw data files must be named after ISIN codes.')
    message('Message: looking index fund data from ../idexfunds')

    wd = getwd()
    path = paste0(wd,path,'/indexfunds')

    indexFiles = list.files(path, full.names = T)
    indexFunds = list()
    for(i in indexFiles) {

        # print(i)
        tmpdf = data.table::fread(i, skip = 1)
        isin  = stringr::str_split(basename(i),pattern = '\\.')[[1]][1]
        tmpdf$isin = isin
        tmpdf$time = parseDoubleQuotesDates(tmpdf$Date)
        tmpdf$nav  = parseDoubleQuotesNumeric(tmpdf$NAV)
        tmpdf$fond = NA

        # BAD bu does the work!
        tmpdf$fond[tmpdf$isin == 'IE0009591805'] = 'Euro Investment Grade Bond Index Fund'
        tmpdf$fond[tmpdf$isin == 'IE00B03HCZ61'] = 'Global Stock Index Fund'

        tmpdf = dplyr::select(tmpdf, isin, fond, time, nav)
        indexFunds[[isin]] = dplyr::arrange(tmpdf, time)
    }
    indexFunds
}

calcStats = function (d) {
    
    # if(nrow(d) == 0 | is.null(nrow(d))) return(NULL)
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

# TEST ----
# setwd("Muu/r-stuff/garage-mudel/")
# source("R/utils.r")
# test = getIndexFundsData(path = "/raw-data"); head(test)
# test = generateDb(path = "/raw-data")
# lapply(test, head)
# 
# # tootlus on see kuidas arvutati xirri
# test[[1]] %>%
#     getStatsTimeWindow(fee = 0.016, start = "2016-01-01", end = Sys.Date())
# 
# # # tootluse muutus ajas - nädalane samm
# dates = as.character(seq.Date(from = as.Date("2000-01-01"), to = Sys.Date(), by = "month"))
# tootlusAjas = list()
# for(i in dates) tootlusAjas[[i]] = getStatsTimeWindow(test[[1]], fee = 0.016, start = i, end = as.Date(i) + months(1))
# tmp = data.table::rbindlist(tootlusAjas)
