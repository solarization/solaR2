fSolI <- function(solD, sample = 'hour', BTi,
                  keep.night = TRUE, eot = TRUE)
{
    #Solar constant
    Bo <- 1367

    if(missing(BTi)){
        d <- solD$Dates
        BTi <- fBTi(d, sample)
    }

    BTi <- data.table(Dates = as.IDate(BTi),
                      Times = as.ITime(BTi))

    #Select the values of solD that are include in BTi
    sun <- solD[BTi]
    sun[, Dates := as.POSIXct(Dates, Times, tz = 'UTC')]
    sun[, Times := NULL]
    setkeyv(sun, c('Dates'))

    #solar time
    sun[, w := sunHour(BTi = BTi, EoT = eot)]

    #classify night elements
    sun[, night := abs(w) >= abs(ws)]
    
    #zenith angle
    sun[, cosThzS := zenith(decl, lat, w)]

    #solar altitude angle
    sun[, AlS := asin(cosThzS)]
    
    #azimuth
    sun[, cosAzS := azimuth(decl, w, lat, AlS)]

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
