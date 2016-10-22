
# lapply(fonds, head)

generateDb = function(path = NULL) {
    
    # Function for generating database from raw data files
    # 
    # Args:
    #  path :  path to raw data folder
    
    # 
    wd = getwd()
    if(!all(list.files(paste0(wd,path)) %in% c("NAV","maht"))) stop("'path' must direct to /raw-data 
                                                         folder with subdirectories 'NAV' and 'maht'")
    
    navFiles  = list.files("raw-data/NAV/", full.names = T)
    mahtFiles = list.files("raw-data/maht/", full.names = T) 
    
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
    
    # make list of fonds
    fonds = list()
    for(i in levels(df$ISIN)) fonds[[i]] = dplyr::filter(df, ISIN == i)
    
    fonds = lapply(fonds, calcStats)
    fonds
} 

# TEST ----
# setwd("Muu/r-stuff/garage-mudel/")
# source("R/utils.r")
# test <- generateDb(path = "raw-data/")
# lapply(test, head)
