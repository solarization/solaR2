calcGef<-function(lat,
                  modeTrk='fixed',      #c('two','horiz','fixed')
                  modeRad='prom', 
                  dataRad,
                  sample='hour',
                  keep.night=TRUE,
                  sunGeometry='michalsky',
                  corr, f,
                  betaLim=90, beta=abs(lat)-10, alfa=0,
                  iS=2, alb=0.2, horizBright=TRUE, HCPV=FALSE,
                  modeShd='',    #modeShd=c('area','bt','prom')
                  struct=list(), #list(W=23.11, L=9.8, Nrow=2, Ncol=8), 
                  distances=data.table(),#data.table(Lew=40, Lns=30, H=0)){
                  ...){
    
    stopifnot(is.list(struct), is.data.frame(distances))
    
    if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
        modeShd[which(modeShd=='bt')]='area'
        warning('backtracking is only implemented for modeTrk=horiz')}
    
    if (modeRad!='prev'){                 #No utilizamos un cálculo prev
        radHoriz <- calcG0(lat=lat, modeRad=modeRad,
                           dataRad=dataRad,
                           sample=sample, keep.night=keep.night,
                           sunGeometry=sunGeometry,
                           corr=corr, f=f, ...)
    } else {#Utilizamos un cálculo prev de calcG0
        radHoriz <- as(dataRad, 'G0') ##OJO: ¿hace falta comprobar que coinciden lat y otras?
    } 
    
###Paso a inclinada y radiación efectiva
    BT=("bt" %in% modeShd) 
    angGen <- fTheta(radHoriz, beta, alfa, modeTrk, betaLim, BT, struct, distances)
    inclin <- fInclin(radHoriz, angGen, iS, alb, horizBright, HCPV)
    
###Valores diarios, mensuales y anuales
    d <- truncDay(inclin$Dates)
    d <- unique(d)
    by <- radHoriz@sample
    
    Gefdm <-  inclin[, .(Bod = P2E(Bo, by)/1000,
                         Bnd = P2E(Bn, by)/1000,
                         Gd = P2E(G, by)/1000,
                         Dd = P2E(D, by)/1000,
                         Gefd = P2E(Gef, by)/1000,
                         Defd = P2E(Def, by)/1000,
                         Befd = P2E(Bef, by)/1000),
                     by = .(month(truncDay(Dates)), year(truncDay(Dates)))]
    

    if(radHoriz@type == 'prom'){
        GefD <- Gefdm[, .(Bod = Bod*1000,
                           Bnd = Bnd*1000,
                           Gd = Gd*1000,
                           Dd = Dd*1000,
                           Gefd = Gefd*1000,
                           Defd = Defd*1000,
                           Befd = Befd*1000),
                      by = d]
        
       
        Gefdm[, DayOfMonth := DOM(Gefdm)]
        Gefy <- Gefdm[, .(Bod = sum(Bod*DayOfMonth, na.rm = TRUE),
                          Bnd = sum(Bnd*DayOfMonth, na.rm = TRUE),
                          Gd = sum(Gd*DayOfMonth, na.rm = TRUE),
                          Dd = sum(Dd*DayOfMonth, na.rm = TRUE),
                          Gefd = sum(Gefd*DayOfMonth, na.rm = TRUE),
                          Defd = sum(Defd*DayOfMonth, na.rm = TRUE),
                          Befd = sum(Befd*DayOfMonth, na.rm = TRUE)),
                      by = year(d)]
        Gefdm[, DayOfMonth := NULL]
    } else{
        
        GefD <-  inclin[, .(Bod = P2E(Bo, by)/1000,
                             Bnd = P2E(Bn, by)/1000,
                             Gd = P2E(G, by)/1000,
                             Dd = P2E(D, by)/1000,
                             Gefd = P2E(Gef, by)/1000,
                             Defd = P2E(Def, by)/1000,
                             Befd = P2E(Bef, by)/1000),
                         by = truncDay(Dates)]
        

        Gefy <- GefD[, .(Bod = sum(Bod, na.rm = TRUE)/1000,
                           Bnd = sum(Bnd, na.rm = TRUE)/1000,
                           Gd = sum(Gd, na.rm = TRUE)/1000,
                           Dd = sum(Dd, na.rm = TRUE)/1000,
                           Gefd = sum(Gefd, na.rm = TRUE)/1000,
                           Defd = sum(Defd, na.rm = TRUE)/1000,
                           Befd = sum(Befd, na.rm = TRUE)/1000),
                       by = year(d)]            
    }

    Gefdm[, Dates := paste(month.abb[month], year, sep = '. ')]
    Gefdm[, c('month', 'year') := NULL]
    setcolorder(Gefdm, c('Dates', names(Gefdm)[-length(Gefdm)]))
    names(Gefy)[1] <- 'Dates'
    names(GefD)[1] <- 'Dates'
    
###Resultado antes de sombras
    result0=new('Gef',
                radHoriz,                           #Gef contains 'G0'
                Theta=angGen,
                GefD=GefD,
                Gefdm=Gefdm,
                Gefy=Gefy,
                GefI=inclin,
                iS=iS,
                alb=alb,
                modeTrk=modeTrk,
                modeShd=modeShd,
                angGen=list(alfa=alfa, beta=beta, betaLim=betaLim),
                struct=struct,
                distances=distances
                )
###Cálculo de sombras
    if (isTRUE(modeShd == "") ||        #Si modeShd=='' no hace calculo de sombras
        ('bt' %in% modeShd)) {            #tampoco si hay backtracking
        return(result0)
    } else {
        result <- calcShd(result0, modeTrk, modeShd, struct, distances)
        return(result)
    }
}

