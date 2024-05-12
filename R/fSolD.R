fSolD <- function(d, lat, method = 'michalsky'){
    if (abs(lat) > 90){
        lat <- sign(lat) * 90
        warning(paste('Latitude outside acceptable values. Set to', lat))
    }
    Dates <- unique(as.IDate(d))

    #### solarAngles ####
    
    ##Declination
    decl <- declination(Dates, method = method)
    ##Eccentricity
    eo <- eccentricity(Dates, method = method)
    ##Equation of time
    EoT <- eot(Dates)
    ##Solar time
    ws <- sunrise(Dates, lat, method = method,
                  decl = decl)
    ##Extraterrestrial irradiance
    Bo0d <- bo0d(Dates, lat, method = method,
                 decl = decl, eo = eo, ws = ws)

    result <- data.table(Dates = Dates,
                         lat = lat,
                         decl = decl,
                         eo = eo,
                         EoT = EoT,
                         ws = ws,
                         Bo0d = Bo0d)
    attr(result, 'lat') = lat
    setkey(result, Dates)
    return(result)
}
