calcSol <- function(d, lat, BTi,
                    sample = 'hour',
                    keep.night = TRUE,
                    method = 'michalsky',
                    EoT = TRUE)
{
    solD <- fSolD(d = d, lat = lat, method = method)
    solI <- fSolI(solD = solD, sample = sample,
                  BTi = BTi, keep.night = keep.night,
                  EoT = EoT)
    
    if(!missing(BTi)){
        sample <- solI$Dates[2]-solI$Dates[1]
        sample <- format(sample)
    }
    
    solD[, lat := NULL]
    solI[, lat := NULL]
    result <- new('Sol',
                  lat = lat,
                  solD = solD,
                  solI = solI,
                  sample = sample,
                  method = method)
    return(result)
}
