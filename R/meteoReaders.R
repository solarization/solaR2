#### monthly means of irradiation ####
readG0dm <- function(lat, G0dm, Ta = 25,
                     year = as.POSIXlt(Sys.Date())$year + 1900,
                     promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13),
                     source = '')
{
    if(missing(lat)){lat <- 0}
    Dates <- as.IDate(paste(year, 1:12, promDays, sep = '-'), tz = 'UTC')
    G0dm.dt <- data.table(Dates = Dates,
                          G0 = G0dm,
                          Ta = Ta)
    setkey(G0dm.dt, 'Dates')
    results <- new(Class = 'Meteo',
                   latm = lat,
                   data = G0dm.dt,
                   type = 'prom',
                   source = source)
}

#### file to Meteo (daily) ####
readBDd <- function(file, lat,
                   format = "%d/%m/%Y",header = TRUE,
                   fill = TRUE, dec = '.', sep = ';',
                   dates.col = 'Dates', ta.col = 'Ta',
                   g0.col = 'G0', keep.cols = FALSE)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #read from file and set it in a data.table
    bd <- fread(file, header = header, fill = fill, dec = dec, sep = sep)

    #name the dates column by Dates
    Dates <- bd[[dates.col]]
    bd[,(dates.col) := NULL]
    bd[, Dates := as.IDate(Dates, format = format)]

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    names0 <- NULL
    if(!('D0' %in% bd) && !('B0' %in% bd)){
        names0 <- 'D0'
        names0[2] <- 'B0'
    }
    
    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', names0, 'Ta'))
    }
    else
    {
        #erase the rest of the columns
        cols <- c('Dates', 'G0', names0, 'Ta')
        bd <- bd[, ..cols]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bd',
                  source = file)
}

#### file to Meteo (intradaily) ####
readBDi <- function(file, lat,
                    format = "%d/%m/%Y %H:%M:%S",
                    header = TRUE, fill = TRUE, dec = '.',
                    sep = ';', dates.col = 'dates', times.col,
                    ta.col = 'Ta', g0.col = 'G0', keep.cols = FALSE)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #read from file and set it in a data.table
    bd <- fread(file, header = header, fill = fill, dec = dec, sep = sep)

    if(!missing(times.col)){
        stopifnot(is.character(times.col) || is.numeric(times.col))
 
        #name the dates column by Dates
        format <- strsplit(format, ' ')
        dd <- as.IDate(bd[[dates.col]], format = format[[1]][1])
        tt <- as.ITime(bd[[times.col]], format = format[[1]][2])
        bd[,(dates.col) := NULL]
        bd[,(times.col) := NULL]
        bd[, Dates := as.POSIXct(dd, tt, tz = 'UTC')]

    }

    else
    {
        dd <- as.POSIXct(bd[[dates.col]], format = format, tz = 'UTC')
        bd[, (dates.col) := NULL]
        bd[, Dates := dd]
    }

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    names0 <- NULL
    if(!('D0' %in% bd) && !('B0' %in% bd)){
        names0 <- 'D0'
        names0[2] <- 'B0'
    }
    
    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', names0, 'Ta'))
    }
    else
    {
        #erase the rest of the columns
        cols <- c('Dates', 'G0', names0, 'Ta')
        bd <- bd[, ..cols]
    }
    
    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bdI',
                  source = file)
}


#### data.frame/table to Meteo (daily)
dt2Meteod <- function(file, lat, source = '')
{
    ##make sure its a data.table
    bd <- data.table(file)

    ## Dates is an as.POSIX element
    bd[, Dates := as.POSIXct(Dates, tz = 'UTC')]
       
    nms0 <- NULL
    if(('D0' %in% bd) && ('B0' %in% bd)){
        nms0 <- 'D0'
        nms0[2] <- 'B0'
    }

    if('Ta' %in% bd){nms0[length(nms0)+1] <- 'Ta'}

    if(('TempMin' %in% bd) && ('TempMax' %in% bd)){
        nms0[length(nms0)+1] <- 'TempMin'
        nms0[length(nms0)+1] <- 'TempMax'
    }
    
    ##reorder the columns
    setcolorder(bd, c('Dates', 'G0', nms0))

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bd',
                  source = source)
}


#### data.frame/table to Meteo (intradaily) ####
dt2Meteoi <- function(file, lat, source = '')
{
    ##make sure its a data.table
    bd <- data.table(file)

    ##Dates is an as.POSIX element
    bd[, Dates := as.POSIXct(Dates, tz = 'UTC')]
    
    nms0 <- NULL
    if(('D0' %in% bd) && ('B0' %in% bd)){
        nms0 <- 'D0'
        nms0[2] <- 'B0'
    }

    if('Ta' %in% bd){
        nms0[length(nms0)+1] <- 'Ta'
    }

    if(('TempMin' %in% bd) && ('TempMax' %in% bd)){
        nms0[length(nms0)+1] <- 'TempMin'
        nms0[length(nms0)+1] <- 'TempMax'
    }
    
    ##reorder the columns
    setcolorder(bd, c('Dates', 'G0', nms0))
   
    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bdI',
                  source = source)
}

#### data.frame/table to Meteo (monthly) ####
dt2Meteom <- function(file, lat, source = '')
{
    ##make sure its a data.table
    bd <- data.table(file)

    ##Dates is an as.POSIX element
    bd[, Dates := as.POSIXct(Dates, tz = 'UTC')]
    
    nms0 <- NULL
    if(('D0d' %in% bd) && ('B0d' %in% bd)){
        nms0 <- 'D0d'
        nms0[2] <- 'B0d'
    }

    if('Ta' %in% bd){
        nms0[length(nms0)+1] <- 'Ta'
    }

    if(('TempMin' %in% bd) && ('TempMax' %in% bd)){
        nms0[length(nms0)+1] <- 'TempMin'
        nms0[length(nms0)+1] <- 'TempMax'
    }
    
    ##reorder the columns
    setcolorder(bd, c('Dates', 'G0d', nms0))
    
    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'prom',
                  source = source)
}


#### Liu and Jordan, Collares-Pereira and Rabl proposals ####
collper <- function(sol, compD)
{
    ind.rep <- indexRep(sol)
    solI <- as.data.tableI(sol, complete = T)
    ws <- solI$ws
    w <- solI$w

    a <- 0.409-0.5016*sin(ws+pi/3)
    b <- 0.6609+0.4767*sin(ws+pi/3)

    rd <- solI[, Bo0/Bo0d]
    rg <- rd * (a + b * cos(w))

    # Daily irradiation components
    G0d <- compD$G0d[ind.rep]
    B0d <- compD$B0d[ind.rep]
    D0d <- compD$D0d[ind.rep]

    # Daily profile
    G0 <- G0d * rg
    D0 <- D0d * rd
    
    # This method may produce diffuse irradiance higher than
    # global irradiance
    G0 <- pmax(G0, D0, na.rm = TRUE)
    B0 <- G0 - D0

    # Negative values are set to NA
    neg <- (B0 < 0) | (D0 < 0) | (G0 < 0)
    is.na(G0) <- neg
    is.na(B0) <- neg
    is.na(D0) <- neg
    
    # Daily profiles are scaled to keep daily irradiation values
    day <- truncDay(indexI(sol))
    sample <- sol@sample

    G0dCP <- ave(G0, day, FUN=function(x) P2E(x, sample))
    B0dCP <- ave(B0, day, FUN=function(x) P2E(x, sample))
    D0dCP <- ave(D0, day, FUN=function(x) P2E(x, sample))
    
    G0 <- G0 * G0d/G0dCP
    B0 <- B0 * B0d/B0dCP
    D0 <- D0 * D0d/D0dCP
    
    res <- data.table(G0, B0, D0)
    return(res)
}


#### intradaily Meteo to daily Meteo ####
Meteoi2Meteod <- function(G0i)
{
    lat <- G0i@latm
    source <- G0i@source

    dt <- getData(G0i)
    dt <- dt[, lapply(.SD, mean), by = as.IDate(Dates)]
    names(dt)[1] <- 'Dates'
    
    G0d <- dt2Meteod(dt, lat, source)
    return(G0d)
}

#### daily Meteo to monthly Meteo ####
Meteod2Meteom <- function(G0d)
{
    lat <- G0d@latm
    source <- G0d@source

    dt <- getData(G0d)
    nms <- names(dt)[-1]
    dt <- dt[, lapply(.SD, mean),
             .SDcols = nms,
             by = .(month(Dates), year(Dates))]
    dt[, Dates := fBTd()]
    dt <- dt[, c('month', 'year') := NULL]
    
    setcolorder(dt, 'Dates')

    G0m <- dt2Meteom(dt, lat, source)
    return(G0m)
}

zoo2Meteo <- function(file, lat, source = '')
{
    sample <- median(diff(index(file)))
    IsDaily <- as.numeric(sample, units = 'days')>=1
    type <- ifelse(IsDaily, 'bd', 'bdI')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = file,
                  type = type,
                  source = source)
}

dt2Meteo <- function(file, lat, source = '', type)
{
    ##Make sure is a data.table
    file <- data.table(file)
    
    stopifnot('Dates' %in% names(file))
    if(!('Ta' %in% names(file)) && !('TempMin' %in% names(file))){
        ## file <- data.table(Dates = file$Dates,
        ##                    G0 = file$G0,
        ##                    Ta = 25)
        file[, Ta := 25]
    }
    sample <- median(diff(file$Dates))
    if(missing(type)){
        IsDaily <- as.numeric(sample, units = 'days')>=1
        type <- ifelse(IsDaily, 'bd', 'bdI')
    }
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = file,
                  type = type,
                  source = source)
}
