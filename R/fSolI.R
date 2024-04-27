fSolI <- function(solD, sample = 'hour', BTi,
                  keep.night = TRUE, EoT = TRUE)
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
    sun[, w := sunHour(BTi, EoT = EoT)]

    #classify night elements
    sun[, night := abs(w) >= abs(ws)]
    
    #zenith angle
    sun[, cosThzS := zenith(decl, lat, w)]

    #solar altitude angle
    sun[, AlS := asin(cosThzS)]
    
    #azimuth
    sun[, cosAzS := azimuth(solD, decl, w, lat, AlS)]

    #Extraterrestrial irradiance
    sun[, Bo0 := Bo * eo * cosThzS]
    
    #When it is night there is no irradiance
    sun[night == TRUE, Bo0 := 0]
    
    #keep night
    if(!keep.night){
        sun <- sun[night == FALSE]
    }

    #Reorder columns so date and time are first an second ones
    setcolorder(sun, c('Dates', names(solD)[-1]))
    
    sun
}
