calcSol <- function(d, lat, lon, BTi,
                    sample = 'hour',
                    keep.night = TRUE,
                    method = 'michalsky',
                    GMT = 0)
{
    solD <- fSolD(d = d, lat = lat, method = method)
    solI <- fSolI(solD = solD, lon = lon, sample = sample,
                  BTi = BTi, GMT = GMT, keep.night = keep.night)
    
    if(!missing(BTi)){
        sample <- solI$Dates[2]-solI$Dates[1]
        sample <- format(sample)
    }
    
    solD[, lat := NULL]
    solI[, lat := NULL]
    solI[, lon := NULL]
    result <- new('Sol',
                  lat = lat,
                  lon = lon,
                  solD = solD,
                  solI = solI,
                  sample = sample,
                  method = method)
    return(result)
}
