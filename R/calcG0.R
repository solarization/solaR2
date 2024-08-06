calcG0 <- function(lat,
                   modeRad='prom',
                   dataRad,
                   sample='hour',
                   keep.night=TRUE,
                   sunGeometry='michalsky',
                   corr, f, ...)
{
    
    if (missing(lat)) stop('lat missing. You must provide a latitude value.')
    
    stopifnot(modeRad %in% c('prom', 'aguiar','bd', 'bdI'))
    

###Datos de Radiacion
    if (missing(corr)){
        corr = switch(modeRad,
                      bd = 'CPR', #Correlacion entre Fd y Kt para valores diarios
                      aguiar = 'CPR', #Correlacion entre Fd y Kt para valores diarios
                      prom = 'Page', #Correlacion entre Fd y Kt para promedios mensuales
                      bdI = 'BRL' #Correlación entre fd y kt para valores intradiarios
                      )
    }
    
    if(is(dataRad, 'Meteo')){BD <- dataRad}
    else{
    BD <- switch(modeRad,
                 bd = {
                         if (!is.list(dataRad)) dataRad <- list(file=dataRad)
                         switch(class(dataRad$file)[1],
                                character={
                                    bd.default=list(file='', lat=lat)
                                    bd=modifyList(bd.default, dataRad)
                                    res <- do.call('readBDd', bd)
                                    res
                                },
                                data.table= ,
                                data.frame={
                                    bd.default=list(file='', lat=lat)
                                    bd=modifyList(bd.default, dataRad)
                                    res <- do.call('dt2Meteo', bd)
                                    res
                                },
                                zoo={
                                    bd.default=list(file='', lat=lat, source='')
                                    bd=modifyList(bd.default, dataRad)
                                    res <- do.call('zoo2Meteo', bd)
                                    res
                                })
                     },                #Fin de bd
                 prom = {
                     if (!is.list(dataRad)) dataRad <- list(G0dm=dataRad)
                     prom.default <- list(G0dm=numeric(), lat=lat)
                     prom = modifyList(prom.default, dataRad)
                     res <- do.call('readG0dm', prom)
                 },                     #Fin de prom
                 aguiar = {
                     if (is.list(dataRad)) dataRad <- dataRad$G0dm
                     BTd <- fBTd(mode='serie')
                     solD <- fSolD(lat, BTd)
                     G0d <- markovG0(G0dm, solD)
                     res <- dt2Meteo(G0d, lat=lat, source='aguiar')
                 },                     #Fin de aguiar
                 bdI = {
                     if (is(dataRad, 'Meteo')) {
                         dataRad
                     } else {
                         if (!is.list(dataRad)) dataRad <- list(file=dataRad)
                         switch(class(dataRad$file)[1],
                                character = {
                                    bdI.default <- list(file='', lat=lat)
                                    bdI <- modifyList(bdI.default, dataRad)
                                    res <- do.call('readBDi', bdI)
                                    res
                                },
                                data.table = ,
                                data.frame = {
                                    bdI.default <- list(file='', lat=lat)
                                    bdI <- modifyList(bdI.default, dataRad)
                                    res <- do.call('dt2Meteo', bdI)
                                    res
                                },
                                zoo = {
                                    bdI.default <- list(file='', lat=lat, source='')
                                    bdI <- modifyList(bdI.default, dataRad)
                                    res <- do.call('zoo2Meteo', bdI)
                                    res
                                },
                                stop('dataRad$file should be a character, a data.table, a data.frame or a zoo.')
                                )}}     #Fin de bdI
                 )                      #Fin del switch general
    
    }
        
    
### Angulos solares y componentes de irradiancia
    if (modeRad=='bdI') {
        sol <- calcSol(lat, sample = sample,
                       BTi = indexD(BD), keep.night=keep.night, method=sunGeometry)
        compI <- fCompI(sol=sol, G0I=BD, corr=corr, f=f, ...)
        compD <- compI[, lapply(.SD, P2E, sol@sample),
                       .SDcols = c('G0', 'D0', 'B0'),
                       by = truncDay(Dates)]
        names(compD)[1] <- 'Dates'
        names(compD)[-1] <- paste(names(compD)[-1], 'd', sep = '')
        compD$Fd <- compD$D0d/compD$G0d
        compD$Kt <- compD$G0d/sol@solD$Bo0d
    } else { ##modeRad!='bdI'
        sol <- calcSol(lat, indexD(BD), sample = sample,
                       keep.night = keep.night, method = sunGeometry)
        compD<-fCompD(sol=sol, G0d=BD, corr=corr, f, ...)
        compI<-fCompI(sol=sol, compD=compD, ...)
    }
    
###Temperatura
    
    ##Compruebo si tengo información de temperatura a partir de la cual
    ##generar una secuencia de datos. Para eso, debo estar leyendo de www.mapa.es
    ##o de una base de datos que contenga dos variables con información sobre
    ##valores diarios máximos y mínimos de temperatura.

    Ta=switch(modeRad,
              bd={
                  if (all(c("TempMax","TempMin") %in% names(BD@data))) {
                      fTemp(sol, BD)
                  } else {
                      if ("Ta" %in% names(BD@data)) {
                          data.table(indexD(sol), BD@data$Ta)
                      } else {
                          warning('No temperature information available!')
                      }
                  }
              },
              bdI={
                  if ("Ta" %in% names(BD@data)) {
                      data.table(indexI(sol), BD@data$Ta)
                  } else {
                      warning('No temperature information available!')
                  }
              },
              prom={
                  if ("Ta" %in% names(BD@data)) {
                      data.table(indexD(sol), BD@data$Ta)
                  } else {
                      warning('No temperature information available!')
                  }                  
              },
              aguiar={ 
                  data.table(indexI(sol), BD@data$Ta)
              }
              )
    names(Ta)[1] <- 'Dates'
    names(Ta)[2] <- 'Ta'
    
###Medias mensuales y anuales
    nms <- c('G0d', 'D0d', 'B0d')
    G0dm <- compD[, lapply(.SD/1000, mean, na.rm = TRUE),
                  .SDcols = nms,
                  by = .(month(Dates), year(Dates))]
    
    if(modeRad == 'prom'){
        G0dm[, DayOfMonth := DOM(G0dm)]
        G0y <- G0dm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                    .SDcols = nms,
                    by = .(Dates = year)]
        G0dm[, DayOfMonth := NULL]        
    } else{
        G0y <- compD[, lapply(.SD/1000, sum, na.rm = TRUE),
                     .SDcols = nms,
                     by = .(Dates = year(Dates))]
    }
    G0dm[, Dates := paste(month.abb[month], year, sep = '. ')]
    G0dm[, c('month', 'year') := NULL]
    setcolorder(G0dm, 'Dates')
    
###Resultado
    result <- new(Class='G0',
                  BD,                     #G0 contains "Meteo"
                  sol,                    #G0 contains 'Sol'
                  G0D=compD,              #resultado de fCompD
                  G0dm=G0dm,          #medias mensuales
                  G0y=G0y,            #valores anuales
                  G0I=compI,          #resultado de fCompI
                  Ta=Ta               #temperatura ambiente
                  )
    return(result)
}
