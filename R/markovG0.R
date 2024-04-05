## Objects loaded at startup from data/MTM.RData
if(getRversion() >= "2.15.1") globalVariables(c(
                  'MTM', ## Markov Transition Matrices
                  'Ktm0', ## Kt limits to choose a matrix from MTM
                  'Ktlim' ## Daily kt range of each matrix.
                  ))
                  
markovG0 <- function(G0dm, solD){
    solD <- copy(solD)#previene error de referencia
    timeIndex <- solD$Dates
    solD[, Bo0dm := mean(Bo0d), by = .(year(Dates), month(Dates))]
    ktm <- merge(G0dm, solD, by = 'Dates')
    ktm <- ktm[, G0/Bo0dm]

    ##Calculate what matrix has to be used
    whichMatrix <- findInterval(ktm, Ktm0, all.inside = TRUE)

    solD[, c('ktd', 'state') := .(0,0)]
    solD$state[1] <- 1
    solD$ktd[1] <- ktm[solD$state[1]]

    iMonth <- month(timeIndex)
    for (i in 2:length(timeIndex)){
        colMonth <- whichMatrix[iMonth[i]]
        rng <- Ktlim[, colMonth]
        classes <- seq(rng[1], rng[2], length=11)
        matMonth <- MTM[(10*colMonth-9):(10*colMonth),]
        ## http://www-rohan.sdsu.edu/~babailey/stat575/mcsim.r
        solD$state[i] <- sample(1:10, size=1, prob=matMonth[solD$state[i-1],])
        solD$ktd[i] <- runif(1, min=classes[solD$state[i]],
                             max=classes[solD$state[i]+1])
    }
    
    solD[, G0dmMarkov := mean(ktd * Bo0d), by = .(year(Dates), month(Dates))]
    ##########
    #fix <- na.locf(G0dm$G0/G0dmMarkov, x=as.POSIXct, xout=timeIndex)
    fix <- G0dm$G0/solD$G0dmMarkov
    for (i in 1:length(fix)){#falta saber que pasa si el primer valor es NA
        if(is.na(fix[i])){fix[i] <- fix[i-1]}
    }

    #G0d <- Ktd * Bo0d * fix
    G0d <- solD[, ktd *Bo0d] * fix
    G0d
  }
