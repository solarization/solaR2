fCompI <- function(sol, compD, G0I,
                   corr = 'EKDh', f,
                   filterG0 = TRUE){
    if(!(corr %in% c('EKDh', 'CLIMEDh', 'BRL', 'user', 'none'))){
        warning('Wrong descriptor of correlation Fd-Ktd. Set EKDh.')
        corr <- 'EKDh'
    }
  
    if(class(sol) != 'Sol'){
        Dates <- unique(as.IDate(sol$Dates))
        lat <- unique(sol$lat)
        N <- length(sol$Dates)
        BTi <- seq(sol$Dates[1], sol$Dates[N], length.out = N)
        sample <- format(difftime(BTi[2], BTi[1]))
        sol <- calcSol(Dates, lat, BTi, sample)
    }

    
    sample <- sol@sample
    night <- sol@solI$night
    Bo0 <- sol@solI$Bo0
    Dates <- indexI(sol)

    ## If instantaneous values are not provided, compD is used instead.
    if (missing(G0I)) { 

        G0I <- collper(sol, compD)
        G0 <- G0I$G0
        B0 <- G0I$B0
        D0 <- G0I$D0

        Fd <- D0/G0
        Kt <- G0/Bo0
        
    } else { ## Use instantaneous values if provided through G0I

        if(class(G0I) != 'Meteo'){
            Dates <- unique(as.IDate(G0I$Dates))
            lat <- unique(G0I$lat)
            G0 <- G0I$G0
            Ta <- G0I$Ta
            dt <- data.table(Dates = Dates,
                             G0 = G0,
                             Ta = Ta)
            dt[, B0 := G0I$B0]
            dt[, D0 := G0I$D0]
            G0I <- dt2Meteod(dt, lat)
        }
    
        if (corr!='none'){
            G0 <- getG0(G0I)
            ## Filter values: surface irradiation must be lower than
            ## extraterrestial; 
            if (filterG0) {is.na(G0) <- (G0 > Bo0)}

            ## Fd-Kt correlation
            Fd <- switch(corr,
                         EKDh = FdKtEKDh(sol, G0I),
                         CLIMEDh = FdKtCLIMEDh(sol, G0I),
                         BRL = FdKtBRL(sol, G0I), 
                         user = f(sol, G0I))

            Kt <- Fd$Kt
            Fd <- Fd$Fd
            D0 <- Fd * G0
            B0 <- G0 - D0

        } else { 
            G0 <- getG0(G0I)
            D0 <- getData(G0I)[['D0']]
            B0 <- getData(G0I)[['B0']]
            ## Filter values: surface irradiation must be lower than
            ## extraterrestial; 
            if (isTRUE(filterG0)) is.na(G0) <- is.na(D0) <- is.na(B0) <- (G0 > Bo0)
      
            Fd <- D0/G0
            Kt <- G0/Bo0
        }
    }
    ## Values outside sunrise-sunset are set to zero
    G0[night] <- D0[night] <- B0[night] <- Kt[night] <- Fd[night] <- 0

    result <- data.table(Dates, Fd, Kt, G0, D0, B0)
    setkey(result, 'Dates')
    result
}
