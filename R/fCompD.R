fCompD <- function(sol, G0d, corr = 'CPR', f)
{
    if(!(corr %in% c('CPR', 'Page', 'LJ', 'EKDd', 'CLIMEDd', 'user', 'none'))){
        warning('Wrong descriptor of correlation Fd-Ktd. Set CPR.')
        corr <- 'CPR'
    }
    if(class(sol) != 'Sol'){
        Dates <- unique(as.IDate(sol$Dates))
        lat <- unique(sol$lat)
        N <- length(sol$Dates)
        BTi <- seq(sol$Dates[1], sol$Dates[N], length.out = N)
        sol <- calcSol(Dates, lat, lon, BTi)
    }
    if(class(G0d) != 'Meteo'){
        Dates <- unique(as.IDate(G0d$Dates))
        lat <- unique(G0d$lat)
        G0 <- G0d$G0
        Ta <- G0d$Ta
        dt <- data.table(Dates = Dates,
                         G0 = G0,
                         Ta = Ta)
        G0d <- dt2Meteod(dt, lat)
    }  

    stopifnot(indexD(sol) == indexD(G0d))
    Bo0d <- sol@solD$Bo0d
    G0 <- getData(G0d)$G0

    is.na(G0) <- (G0>Bo0d)

    ### the Direct and Difuse data is not given
    if(corr != 'none'){
        Fd <- switch(corr,
                     CPR = FdKtCPR(sol, G0d),
                     Page = FdKtPage(sol, G0d),
                     LJ = FdKtLJ(sol, G0d),
                     CLIMEDd = FdKtCLIMEDd(sol, G0d),
                     user = f(sol, G0d))
        Kt <- Fd$Kt
        Fd <- Fd$Fd
        D0d <- Fd * G0
        B0d <- G0 - D0d
    }
    ### the Direct and Difuse data is given
    else {
        G0 <- getData(G0d)$G0
        D0d <- getData(G0d)[['D0']]
        B0d <- getData(G0d)[['B0']]
        Fd <- D0d/G0
        Kt <- G0/Bo0d
    }

    result <- data.table(Dates = indexD(sol), Fd, Kt, G0d = G0, D0d, B0d)
    setkey(result, 'Dates')
    result
}
