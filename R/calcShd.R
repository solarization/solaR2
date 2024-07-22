calcShd<-function(radEf,##class='Gef'
                  modeTrk='fixed',     #c('two','horiz','fixed')
                  modeShd='prom',      #modeShd=c('area','bt','prom')
                  struct=list(), #list(W=23.11, L=9.8, Nrow=2, Ncol=8), 
                  distances=data.table() #data.table(Lew=40, Lns=30, H=0)){
                  )
{
    stopifnot(is.list(struct), is.data.frame(distances))

    ##Por ahora sólo utilizo modeShd='area'
    ##Con diferentes modeShd (por definir) podré calcular Gef de diferente forma
    ##Ver tesis de Macagnan
    prom=("prom"  %in%  modeShd)
    prev <- as.data.tableI(radEf, complete=TRUE)
    ## Cálculo de sombras
    sol <- data.table(AzS = prev$AzS,
                      AlS = prev$AlS)
    theta <- radEf@Theta
    AngGen <- data.table(theta, sol)
    FS <- fSombra(AngGen, distances, struct, modeTrk, prom)
    ## Cálculo de irradiancia
    gef0 <- radEf@GefI
    Bef0 <- gef0$Bef
    Dcef0 <- gef0$Dcef
    Gef0 <- gef0$Gef
    Dief0 <- gef0$Dief
    Ref0 <- gef0$Ref
    ##Cálculos
    Bef <- Bef0*(1-FS)
    Dcef <- Dcef0*(1-FS)
    Def <- Dief0+Dcef
    Gef <- Dief0+Ref0+Bef+Dcef               #Incluyendo sombras
    ##Cambio nombres
    nms <- c('Gef', 'Def', 'Dcef', 'Bef')
    nmsIndex <- which(names(gef0) %in% nms)
    names(gef0)[nmsIndex]<- paste(names(gef0)[nmsIndex], '0', sep='')
    ##Entrego zoo con resultados, incluyendo previos sin sombras
    ##GefShd=CBIND(gef0, data.frame(Gef, Def, Bef, Dcef, FS), index=indexI(radEf))
    GefShd <- gef0
    GefShd[, c(nms, 'FS') := .(Gef, Def, Dcef, Bef, FS)]

    ## Valores diarios, mensuales y anuales
    d <- truncDay(GefShd$Dates)
    d <- unique(d)
    by <- radEf@sample
    nms <- c('Gef0', 'Def0', 'Bef0', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')
    nmsd <- paste(nms, 'd', sep = '')

    Gefdm <- GefShd[, lapply(.SD/1000, P2E, by),
                    by = .(month(truncDay(Dates)), year(truncDay(Dates))),
                    .SDcols = nms]
    names(Gefdm)[-c(1, 2)] <- nmsd

    if(radEf@type == 'prom'){
        GefD <- Gefdm[, .SD[, -c(1, 2)] * 1000, by = d]
       
        Gefdm[, DayOfMonth := DOM(Gefdm)]
       
        Gefy <- Gefdm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                      by = year(d),
                      .SDcols = nmsd]
        Gefdm[, DayOfMonth := NULL]
    } else{    
        GefD <- GefShd[, lapply(.SD/1000, P2E, by),
                       by = truncDay(Dates),
                       .SDcols = nms]
        names(GefD)[-1] <- nmsd
            
        Gefy <- GefD[, lapply(.SD[, -1], sum, na.rm = TRUE), by = year(d)]
    }

    Gefdm[, Dates := paste(month.abb[month], year, sep = '. ')]
    Gefdm[, c('month', 'year') := NULL]
    setcolorder(Gefdm, c('Dates', names(Gefdm)[-length(Gefdm)]))
    names(Gefy)[1] <- 'Dates'
    names(GefD)[1] <- 'Dates'

    ## Entrego un objecto de clase Gef
    ## modificando los slots 'modeShd', 'GefI', 'GefD', 'Gefdm', y 'Gefy'
    ## del objecto original radEf
    radEf@modeShd=modeShd
    radEf@GefI=GefShd
    radEf@GefD=GefD
    radEf@Gefdm=Gefdm
    radEf@Gefy=Gefy
    return(radEf)
}
  
