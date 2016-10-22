
# lapply(fonds, head)

generateDb = function(path = NULL) {
    
    # Function for generating database from raw data files
    # 
    # Args:
    #  path :  path to raw data folder
    
    # 
    message("Message: working directory is expected to contain folder /raw-data. Parameter 'path' should lead to /raw-data")
    
    wd = getwd()
    path = paste0(wd,path)
    if(!all(c("NAV","maht","indexfunds") %in% list.files(path))) stop("'path' must direct to /raw-data 
                                                         folder with subdirectories 'NAV' and 'maht'")
    # print("siin 1")
    navFiles  = list.files(paste0(path,"/NAV"), full.names = T)
    # print("siin 2")
    mahtFiles = list.files(paste0(path,"/maht"), full.names = T) 
    
    # read nav files
    nav = list()
    for(i in navFiles) nav[[i]] <- readFile(i)
    nav = data.table::rbindlist(nav)
    
    # read maht 
    maht = list()
    for(j in mahtFiles) maht[[j]] <- readFile(j)
    # lapply(maht,head)
    maht = data.table::rbindlist(maht)
    
    # merged data
    df = merge(nav, maht, by = c("Kuupäev", "Fond", "Lühinimi", "ISIN"))
    
    df$time   = parseDate(df$Kuupäev)
    df$nav    = parseFloat(df$NAV)
    df$volume = df$Maht

    # make list of fonds
    fonds = list()
    for(i in levels(df$ISIN)) fonds[[i]] = dplyr::filter(df, ISIN == i)
    
    fonds = lapply(fonds, calcStats)
    message("Message: deleted rows where volume == 0")
    fonds = lapply(fonds, function(x) x[!x$volume == 0 & (!is.na(x$volume)),])
    
    fonds
} 

getIndexFundsData <- function(path = NULL){
    
    message("Message: index fund raw data files must be named after ISIN codes.")
    message("Message: looking index fund data from ../idexfunds")
    
    wd = getwd()
    path = paste0(wd,path,"/indexfunds")
    
    indexFiles = list.files(path, full.names = T)
    indexFunds = list()
    for(i in indexFiles){
        
        # print(i)
        tmpdf = data.table::fread(i, skip = 1)
        isin  = stringr::str_split(basename(i),pattern = "\\.")[[1]][1]
        tmpdf$isin = isin
        tmpdf$time = parseDoubleQuotesDates(tmpdf$Date)
        tmpdf$nav  = parseDoubleQuotesNumeric(tmpdf$NAV)
        tmpdf$fond = NA 
        
        # BAD bu does the work!
        tmpdf$fond[tmpdf$isin == "IE0009591805"] = "Euro Investment Grade Bond Index Fund"
        tmpdf$fond[tmpdf$isin == "IE00B03HCZ61"] = "Global Stock Index Fund"

        tmpdf = dplyr::select(tmpdf, isin, fond, time, nav)
        indexFunds[[isin]] = dplyr::arrange(tmpdf, time)
    }
    indexFunds
}
# TEST ----
# setwd("Muu/r-stuff/garage-mudel/")
# source("R/utils.r")
# test = getIndexFundsData(path = "/raw-data"); head(test)
# test = generateDb(path = "/raw-data")
# lapply(test, head)
