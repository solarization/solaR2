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
                  distances=data.frame(),#data.frame(Lew=40, Lns=30, H=0)){
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
    DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31) ###OJO
    
    Gefdm <-  inclin[, .(Bod = mean(Bo, na.rm = TRUE)/1000,
                         Bnd = mean(Bn, na.rm = TRUE)/1000,
                         Gd = mean(G, na.rm = TRUE)/1000,
                         Dd = mean(D, na.rm = TRUE)/1000,
                         Gefd = mean(Gef, na.rm = TRUE)/1000,
                         Defd = mean(Def, na.rm = TRUE)/1000,
                         Befd = mean(Bef, na.rm = TRUE)/1000),
                     by = .(month(Dates), year(Dates))]
    Gefdm[, Dates := paste(month.abb[month], 'of', year)]
    Gefdm[, c('month', 'year') := NULL]
    setcolorder(Gefdm, c('Dates', names(Gefdm)[-length(Gefdm)]))

    if(radHoriz@type == 'prom'){
        GefD <- Gefdm[, .(Bod = Bod*1000,
                           Bnd = Bnd*1000,
                           Gd = Gd*1000,
                           Dd = Dd*1000,
                           Gefd = Gefd*1000,
                           Defd = Defd*1000,
                           Befd = Befd*1000),
                       by = Dates]

        Gefy <- Gefdm[, .(Bod = sum(Bod*DayOfMonth, na.rm = TRUE),
                          Bnd = sum(Bnd*DayOfMonth, na.rm = TRUE),
                          Gd = sum(Gd*DayOfMonth, na.rm = TRUE),
                          Dd = sum(Dd*DayOfMonth, na.rm = TRUE),
                          Gefd = sum(Gefd*DayOfMonth, na.rm = TRUE),
                          Defd = sum(Defd*DayOfMonth, na.rm = TRUE),
                          Befd = sum(Befd*DayOfMonth, na.rm = TRUE)),
                      by = year(Dates)]
    } else{
        GefD <- inclin[, .(Bod = sum(Bo, na.rm = TRUE),
                           Bnd = sum(Bn, na.rm = TRUE),
                           Gd = sum(G, na.rm = TRUE),
                           Dd = sum(D, na.rm = TRUE),
                           Gefd = sum(Gef, na.rm = TRUE),
                           Defd = sum(Def, na.rm = TRUE),
                           Befd = sum(Bef, na.rm = TRUE)),
                       by = truncDay(Dates)]

        Gefy <- inclin[, .(Bod = sum(Bo, na.rm = 1)/1000,
                           Bnd = sum(Bn, na.rm = 1)/1000,
                           Gd = sum(G, na.rm = 1)/1000,
                           Dd = sum(D, na.rm = 1)/1000,
                           Gefd = sum(Gef, na.rm = 1)/1000,
                           Defd = sum(Def, na.rm = 1)/1000,
                           Befd = sum(Bef, na.rm = 1)/1000),
                       by = year(Dates)]            
    }
    names(Gefy)[1] <- 'Dates'
    
  #if (radHoriz@type=='prom') {
    #Gefdm=aggregate(inclin[,c('Bo', 'Bn', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')]/1000,
      #by=as.yearmon, FUN=P2E, radHoriz@sample) #kWh
    #names(Gefdm)=paste(names(Gefdm), 'd', sep='')

    #GefD=Gefdm*1000                  #Wh
    #index(GefD) <- indexD(radHoriz)  ##para que sea compatible con G0D
    
    #Gefy=zoo(t(colSums(Gefdm*DayOfMonth)),
      #unique(year(index(Gefdm))))
  #} else {
    #GefD=aggregate(inclin[,c('Bo','Bn', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')],
      #by=truncDay, FUN=P2E, radHoriz@sample) #Wh
    #names(GefD)=paste(names(GefD), 'd', sep='')

    #Gefdm=aggregate(GefD/1000, by=as.yearmon, mean, na.rm=1)
    #Gefy=aggregate(GefD/1000, by=year, sum, na.rm=1)
  #}


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

