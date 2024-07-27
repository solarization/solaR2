fSolI <- function(solD, sample = 'hour', BTi,
                  keep.night = TRUE, et = TRUE, method = 'michalsky')
{
    #Solar constant
    Bo <- 1367

    if(missing(BTi)){
        d <- solD$Dates
        BTi <- fBTi(d, sample)
    }
    sun <- data.table(Dates = as.IDate(BTi),
                      Times = as.ITime(BTi))
    sun <- merge(solD, sun, by = 'Dates')

    #sun hour angle
    sun[, w := sunHour(Dates, BTi, EoT = et, method = method, ET = EoT)]

    #classify night elements
    sun[, night := abs(w) >= abs(ws)]
    
    #zenith angle
    sun[, cosThzS := zenith(Dates, lat, BTi,
                            decl = decl,
                            w = w
                            )]

    #solar altitude angle
    sun[, AlS := asin(cosThzS)]
    
    #azimuth
    sun[, AzS := azimuth(Dates, lat, BTi, sample,
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

    #Column Dates with Times
    sun[, Dates := as.POSIXct(Dates, Times, tz = 'UTC')]
    sun[, Times := NULL]
    
    #keep night
    if(!keep.night){
        sun <- sun[night == FALSE]
    }

    return(sun)
}
