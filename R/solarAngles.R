#### Declination ####
declination <- function(d, method = 'michalsky')
{
    ##Method comprobation
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    ## x is an IDate
    d <- as.IDate(d)
    ## Day of year
    dn <- yday(d)
    ## Days from 2000-01-01
    origin <- as.IDate('2000-01-01')
    jd <- as.numeric(d - origin)
    X <- 2 * pi * (dn - 1) / 365

    switch(method,
           michalsky = {
           meanLong <- (280.460 + 0.9856474 * jd)%%360
           meanAnomaly <- (357.528 + 0.9856003 * jd)%%360
           eclipLong <- (meanLong +1.915 * sin(d2r(meanAnomaly)) +
                         0.02 * sin(d2r(2 * meanAnomaly)))%%360
           excen <- 23.439 - 0.0000004 * jd
           sinEclip <- sin(d2r(eclipLong))
           sinExcen <- sin(d2r(excen))
           asin(sinEclip * sinExcen)
           },
           cooper = {
               ##P.I. Cooper. “The Absorption of Solar Radiation in Solar Stills”. Solar Energy 12 (1969).
               d2r(23.45) * sin(2 * pi * (dn +284) / 365)
           },
           strous = {
               meanAnomaly <- (357.5291 + 0.98560028 * jd)%%360
               coefC <- c(1.9148, 0.02, 0.0003)
               sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
               C <- colSums(coefC * sinC)
               trueAnomaly <- (meanAnomaly + C)%%360
               eclipLong <- (trueAnomaly + 282.9372)%%360
               excen <- 23.435
               sinEclip <- sin(d2r(eclipLong))
               sinExcen <- sin(d2r(excen))
               asin(sinEclip * sinExcen)
           },
           spencer = {
               ## J.W. Spencer. “Fourier Series Representation of the Position of the Sun”. 2 (1971).
               ##URL: http://www.mail-archive.com/sundial@uni-koeln.de/msg01050.html.
               0.006918 - 0.399912 * cos(X) + 0.070257 * sin(X) -
                   0.006758 * cos(2 * X) + 0.000907 * sin(2 * X) -
                       0.002697 * cos(3 * X) + 0.001480 * sin(3 * X)           
           })
}


#### Eccentricity ####
eccentricity <- function(d, method = 'michalsky')
{
        ##Method comprobation
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }
    
    ##x is an IDate
    d <- as.IDate(d)
    ##Day of year
    dn <- yday(d)
    X <- 2 * pi * (dn-1)/365

    switch(method,
           cooper = 1 + 0.033*cos(2*pi*dn/365),
           spencer = , 
           michalsky = , 
           strous = 1.000110 + 0.034221*cos(X) +
               0.001280*sin(X) + 0.000719*cos(2*X) +
               0.000077*sin(2*X)
           )
}


#### Equation of time

##Alan M.Whitman "A simple expression for the equation of time"
##EoT=ts-t, donde ts es la hora solar real y t es la hora solar
##media. Valores negativos implican que el sol real se retrasa
##respecto al medio
eot <- function(d)
{
    ## d in an IDate
    d <- as.IDate(d)
    ## Day of year
    dn <- yday(d)
    M <- 2 * pi/365.24 * dn
    EoT <- 229.18 * (-0.0334 * sin(M) +
                         0.04184 * sin(2 * M + 3.5884))
    return(EoT)
}


#### Solar time ####
sunrise <- function(d, lat, ...,
                    decl = declination(d, ...))
{
    cosWs <- -tan(d2r(lat)) * tan(decl)
    #sunrise, negative since it is before noon
    ws <- suppressWarnings(-acos(cosWs))
    #Polar day/night
    polar <- which(is.nan(ws))
    ws[polar] <- -pi * (cosWs[polar] < -1) + 0 * (cosWs[polar] > 1)
    ws
}

#### Extraterrestrial irradition ####
bo0d <- function(d, lat, ...,
                 decl = declination(d, ...),
                 eo = eccentricity(d, ...),
                 ws = sunrise(d, lat, ...))
{
    #solar constant
    Bo <- 1367
    lat <- d2r(lat)
    #The negative sign due to the definition of ws
    Bo0d <- -24/pi * Bo * eo * (ws * sin(lat) * sin(decl) +
                                cos(lat) * cos(decl) * sin(ws))
    Bo0d
}


#### Sun hour angle ####
sunHour <- function(BTi, EoT = TRUE)
{
    if (inherits(BTi, 'data.table')) {
            tt <- BTi[, as.POSIXct(Dates, Times, tz = 'UTC')]
            Times <- BTi$Times
            Dates <- BTi$Dates
        }
    else {
        tt <- as.POSIXct(BTi, tz = 'UTC')
        Times <- as.ITime(tt)
        Dates <- as.IDate(tt)
        }
        
    TO <- as.numeric(Times)/3600
    if(EoT){eot <- eot(Dates)
    } else {eot <- 0}
    w <- 15 * (TO - 12) + eot/4
    return(d2r(w))
}

#### zenith angle ####
zenith <- function(decl, lat, w)
{
    lat <- d2r(lat)
    zenith <- sin(decl) * sin(lat) +
        cos(decl) * cos(w) * cos(lat)
    zenith <- ifelse(zenith > 1, 1, zenith)
    return(zenith)
}

#### azimuth ####
azimuth <- function(decl, w, lat, AlS)
{
    signLat <- ifelse(sign(lat) == 0, 1, sign(lat)) #if the sign of lat is 0, it changes it to 1
    lat <- d2r(lat)
    azimuth <- signLat * (cos(decl) * cos(w) * sin(lat) -
                          cos(lat) * sin(decl)) / cos(AlS)
    azimuth <- ifelse(abs(azimuth) > 1, 1 * sign(azimuth), azimuth)
    return(azimuth)
}
