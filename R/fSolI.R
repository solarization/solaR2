fSolI <- function(solD, sample = 'hour', BTi,
                  keep.night = TRUE, et = TRUE, method = 'michalsky')
{
    #Solar constant
    Bo <- 1367

    if(missing(BTi)){
        d <- solD$Dates
        BTi <- fBTi(d, sample)
    }
    x <- as.Date(BTi)
    rep <- cumsum(c(1, diff(x) != 0))

    #Select the values of solD that are include in BTi
    sun <- solD[rep]
    Times <- as.ITime(BTi)
    sun[, Dates := as.POSIXct(Dates, Times, tz = 'UTC')]
    setkeyv(sun, c('Dates'))

    d <- unique(truncDay(BTi))

    #sun hour angle
    sun[, w := sunHour(d, BTi, EoT = et, method = method)]

    #classify night elements
    sun[, night := abs(w) >= abs(ws)]
    
    #zenith angle
    sun[, cosThzS := zenith(d, lat, BTi,
                            decl = decl,
                            w = w
                            )]

    #solar altitude angle
    sun[, AlS := asin(cosThzS)]
    
    #azimuth
    sun[, AzS := azimuth(d, lat, BTi, sample,
                            decl = decl, 
                            w = w,
                            cosThzS = cosThzS)]

    #Extraterrestrial irradiance
    sun[, Bo0 := Bo * eo * cosThzS]
    
    #When it is night there is no irradiance
    sun[night == TRUE, Bo0 := 0]

    #Erase columns that are in solD
    sun[, decl := NULL]
    sun[, eo := NULL]
    sun[, EoT := NULL]
    sun[, ws := NULL]
    sun[, Bo0d := NULL]
    
    #keep night
    if(!keep.night){
        sun <- sun[night == FALSE]
    }

    return(sun)
}
