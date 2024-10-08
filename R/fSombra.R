fSombra<-function(angGen, distances, struct, modeTrk='fixed',prom=TRUE){

    stopifnot(modeTrk %in% c('two','horiz','fixed'))
    res <- switch(modeTrk, 
                  two = {fSombra6(angGen, distances, struct, prom)},
                  horiz = {fSombraHoriz(angGen, distances, struct)},
                  fixed = {fSombraEst(angGen, distances, struct)}
                  )
    return(res)
}

