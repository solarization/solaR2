#### monthly means of irradiation ####
readG0dm <- function(G0dm, lat, Ta = 25,
                     year = as.numeric(format(Sys.Date(), '%Y')),
                     promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13),
                     source = '')
{
    Dates <- as.IDate(paste(year, 1:12, promDays, sep = '-'), tz = 'UTC')
    G0dm <- as.numeric(G0dm)
    Ta <- as.numeric(Ta)
    G0dm.dt <- data.table(Dates = Dates,
                          G0 = G0dm,
                          Ta = Ta)
    setkey(G0dm.dt, 'Dates')
    results <- new(Class = 'Meteo',
                   lat = lat,
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

    #names of the rest of the columns
    cols <- names(bd)[!(names(bd) %in% c('Dates', 'G0', 'Ta'))]

    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', 'Ta', cols))
    }
    else
    {
        #erase the rest of the columns
        bd[, (cols) := NULL]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  lat = lat,
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

    #names of the rest of the columns
    cols <- names(bd)[!(names(bd) %in% c('Dates', 'G0', 'Ta'))]

    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', 'Ta', cols))
    }
    else
    {
        #erase the rest of the columns
        bd[, (cols) := NULL]
    }
    
    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  lat = lat,
                  data = bd,
                  type = 'bdI',
                  source = file)
}


#### data.frame/table to Meteo (daily)
dt2Meteod <- function(file, lat, source = '',
                    format = "%Y-%m-%d", dates.col = 'Dates',
                    ta.col = 'Ta', g0.col = 'G0', keep.cols = FALSE)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #make sure its a data.table
    bd <- data.table(file)
    
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

    #names of the rest of the columns
    cols <- names(bd)[!(names(bd) %in% c('Dates', 'G0', 'Ta'))]

    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', 'Ta', cols))
    }
    else
    {
        #erase the rest of the columns
        bd[, (cols) := NULL]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  lat = lat,
                  data = bd,
                  type = 'bdI',
                  source = source)
}


#### data.frame/table to Meteo (intradaily) ####
dt2Meteoi <- function(file, lat, source = '',
                    format = "%d-%m-%Y %H:%M:%S",
                    dates.col = 'Dates', ta.col = 'Ta',
                    g0.col = 'G0', keep.cols = FALSE)
{
        #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #make sure its a data.table
    bd <- data.table(file)
    
    #name the dates column by Dates
    Dates <- bd[[dates.col]]
    bd[,(dates.col) := NULL]
    bd[, Dates := as.POSIXct(Dates, format = format, tz = 'UTC')]

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    #names of the rest of the columns
    cols <- names(bd)[!(names(bd) %in% c('Dates', 'G0', 'Ta'))]

    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', 'Ta', cols))
    }
    else
    {
        #erase the rest of the columns
        bd[, (cols) := NULL]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  lat = lat,
                  data = bd,
                  type = 'bd',
                  source = source)
}

#### data.frame/table to Meteo (monthly) ####
dt2Meteom <- function(file, lat, source = '',
                      format = '%Y-%m-%d', dates.col = 'Dates',
                      ta.col = 'Ta', g0.col = 'G0', keep.cols = FALSE)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #make sure its a data.table
    bd <- data.table(file)

    #name the dates column by Dates
    Dates <- bd[[dates.col]]
    bd[, (dates.col) := NULL]
    bd[, Dates := as.IDate(Dates, format = format)]

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    #names of the rest of the columns
    cols <- names(bd)[!(names(bd) %in% c('Dates', 'G0', 'Ta'))]

    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', 'Ta', cols))
    }
    else
    {
        #erase the rest of the columns
        bd[, (cols) := NULL]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  lat = lat,
                  data = bd,
                  type = 'bdM',
                  source = source)
}


#### Liu and Jordan, Collares-Pereira and Rabl proposals ####
collper <- function(sol, compD)
{
    ws <- sol@solI$ws
    w <- sol@solI$w

    a <- 0.409-0.5016*sin(ws+pi/3)
    b <- 0.6609+0.4767*sin(ws+pi/3)

    rd <- sol@solI$Bo0/sol@solD$Bo0d
    rg <- rd * (a + b * cos(w))

    # Daily irradiation components
    G0d <- compD$G0d
    B0d <- compD$B0d
    D0d <- compD$D0d

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
    
    data.table(G0, B0, D0)
}


#### intradaily Meteo to daily Meteo ####
Meteoi2Meteod <- function(G0i)
{
    lat <- G0i@lat
    source <- G0i@source

    dt <- getData(G0i)
    dt <- dt[, lapply(.SD, mean), by = as.IDate(Dates)]
    
    G0d <- dt2Meteod(dt, lat, source, dates.col = 'as.IDate', keep.cols = TRUE)
    return(G0d)
}

#### daily Meteo to monthly Meteo ####
Meteod2Meteom <- function(G0d)
{
    lat <- G0d@lat
    source <- G0d@source

    dt <- getData(G0d)
    dt <- dt[, lapply(.SD, mean), by = .(month(Dates), year(Dates))]
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    dt <- dt[, Dates := as.Date(paste(year, month, promDays, sep = '-'), format = '%Y-%m-%d')]
    dt <- dt[, c('month', 'year') := NULL]
    
    setcolorder(dt, c('Dates', names(dt)[-length(dt)]))

    G0m <- dt2Meteom(dt, lat, source, keep.cols = TRUE)
    return(G0m)
}

zoo2Meteo <- function(file, lat, source = '')
{
    sample <- median(diff(index(file)))
    IsDaily <- as.numeric(sample, units = 'days')>=1
    type <- ifelse(IsDaily, 'bd', 'bdI')
    result <- new(Class = 'Meteo',
                  lat = lat,
                  data = bd,
                  type = type,
                  source = source)
}
