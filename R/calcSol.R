calcSol <- function(lat, BTd, BTi,
                    sample = 'hour',
                    keep.night = TRUE,
                    method = 'michalsky',
                    et = TRUE)
{
    if(missing(BTd)) BTd <- truncDay(BTi)
    solD <- fSolD(lat, BTd, method = method)
    solI <- fSolI(solD = solD, sample = sample,
                  BTi = BTi, keep.night = keep.night,
                  et = et)
    
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
