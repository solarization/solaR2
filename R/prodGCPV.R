prodGCPV<-function(lat,
                   modeTrk='fixed', 
                   modeRad='prom',
                   dataRad,
                   sample='hour',
                   keep.night=TRUE,
                   sunGeometry='michalsky',
                   corr, f,
                   betaLim=90, beta=abs(lat)-10, alfa=0,
                   iS=2, alb=0.2, horizBright=TRUE, HCPV=FALSE,
                   module=list(), 
                   generator=list(),
                   inverter=list(), 
                   effSys=list(), 
                   modeShd='',    
                   struct=list(), 
                   distances=data.table(),
                   ...){
    
    stopifnot(is.list(module),
              is.list(generator),
              is.list(inverter),
              is.list(effSys),
              is.list(struct),
              is.data.table(distances))
    
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
      modeShd[which(modeShd=='bt')]='area'
      warning('backtracking is only implemented for modeTrk=horiz')}
    
    if (modeRad!='prev'){               #No utilizamos un cálculo previo
        
    radEf<-calcGef(lat=lat, modeTrk=modeTrk, modeRad=modeRad,
                   dataRad=dataRad,
                   sample=sample, keep.night=keep.night,
                   sunGeometry=sunGeometry,
                   corr=corr, f=f,
                   betaLim=betaLim, beta=beta, alfa=alfa,
                   iS=iS, alb=alb, horizBright=horizBright, HCPV=HCPV,
                   modeShd=modeShd, struct=struct, distances=distances, ...)
        
  } else { #Utilizamos un cálculo previo de calcG0, calcGef o prodSFCR
      
      stopifnot(class(dataRad) %in% c('G0', 'Gef', 'ProdGCPV'))
      radEf <- switch(class(dataRad),
                      G0=calcGef(lat=lat,
                                 modeTrk=modeTrk, modeRad='prev',
                                 dataRad=dataRad,
                                 betaLim=betaLim, beta=beta, alfa=alfa,
                                 iS=iS, alb=alb, horizBright=horizBright, HCPV=HCPV,
                                 modeShd=modeShd, struct=struct, distances=distances, ...),
                      Gef=dataRad,
                      ProdGCPV=as(dataRad, 'Gef')
                      )
  }
    
    
    ##Producción 
    ##=======================================
    
    prodI<-fProd(radEf,module,generator,inverter,effSys)
    module=attr(prodI, 'module')
    generator=attr(prodI, 'generator')
    inverter=attr(prodI, 'inverter')
    effSys=attr(prodI, 'effSys')
  
    ##Cálculo de valores diarios, mensuales y anuales
  ##=======================================
    Pg=generator$Pg                                   #Wp
    
    by <- radEf@sample
    nms1 <- c('Pac', 'Pdc')
    nms2 <- c('Eac', 'Edc', 'Yf')
    
    ## prodDm <- prodI[, .(Eac = P2E(Pac, by)/1000,
    ##                     Edc = P2E(Pdc, by)/1000),
    ##                 by = .(month(truncDay(Dates)), year(truncDay(Dates)))]
    prodDm <- prodI[, lapply(.SD/1000, P2E, by),
                    .SDcols = nms1,
                    by = .(month(Dates), year(Dates))]
    names(prodDm)[-c(1,2)] <- nms2[-3]
    prodDm[, Yf := Eac/(Pg/1000)]
    
    if(radEf@type == 'prom'){
        prodD <- prodDm[, .SD*1000,
                        .SDcols = nms2,
                        by = indexD(radEf)]
        prodD[, Yf := Yf/1000]
        
        prodDm[, DayOfMonth := DOM(prodDm)]
        ## prody <- prodDm[, .(Eac = sum(Eac*DayOfMonth, na.rm = TRUE),
        ##                     Edc = sum(Edc*DayOfMonth, na.rm = TRUE),
        ##                     Yf = sum(Yf*DayOfMonth, na.rm = TRUE)),
        ##                 by = year(d)]
        prody <- prodDm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                        .SDcols = nms2,
                        by = year(indexD(radEf))]
        prodDm[, DayOfMonth := NULL]
    } else {
        ## prodD <- prodI[, .(Eac = P2E(Pac, by),
        ##                    Edc = P2E(Pdc, by)),
        ##                by = truncDay(Dates)]
        prodD <- prodI[, lapply(.SD, P2E, by),
                       .SDcols = nms1,
                       by = truncDay(Dates)]
        names(prodD)[-1] <- nms2[-3]
        prodD[, Yf := Eac/Pg]
        
        ## prody <- prodD[, .(Eac = sum(Eac, na.rm = TRUE)/1000,
        ##                  Edc = sum(Edc, na.rm = TRUE)/1000,
        ##                  Yf = sum(Yf, na.rm = TRUE)),
        ##              by = year(d)]
        prody <- prodD[, lapply(.SD/1000, sum, na.rm = TRUE),
                       .SDcols = nms2,
                       by = year(indexD(radEf))]
        prody[, Yf := Yf * 1000]
    }
    
    prodDm[, Dates := paste(month.abb[month], year, sep = '. ')]
    prodDm[, c('month', 'year') := NULL]
    setcolorder(prodDm, c('Dates', names(prodDm)[-length(prodDm)]))
    names(prody)[1] <- 'Dates'
    names(prodD)[1] <- 'Dates'
    
    result <- new('ProdGCPV',
                  radEf,                  #contains 'Gef'
                  prodD=prodD,
                  prodDm=prodDm,
                  prody=prody,
                  prodI=prodI,
                  module=module,
                  generator=generator,
                  inverter=inverter,
                  effSys=effSys
                  )
}
