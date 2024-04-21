intervalo <- function(day, sample){
    intervalo <- seq.POSIXt(from = as.POSIXct(paste(day, '00:00:00'), tz = 'UTC'),
                      to = as.POSIXct(paste(day, '23:59:59'), tz = 'UTC'),
                      by = sample)
    return(intervalo)
}

fBTi <- function(d, sample){
    BTi <- lapply(d, intervalo, sample)
    BTi <- unlist(BTi)
    BTi <- as.POSIXct(BTi)
    return(BTi)
}

