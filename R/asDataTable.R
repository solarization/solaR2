### as.data.tableI ###
## Extracts a data.table with intradaily data ##
setGeneric('as.data.tableI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.tableI')})

setMethod('as.data.tableI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
              sol <- copy(object)
              BTi <- indexI(sol)
              BTi <- truncDay(BTi)
              ind.rep <- cumsum(c(1, diff(BTi, units='days')!=0))
              solI <- sol@solI
              solD <- sol@solD[ind.rep]
              if(complete){
                  data <- data.table(solI, solD[, Dates := NULL])
              } else{data <- solI}
              if(day){
                  ind <- indexI(sol)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableI',
          signature = (object='G0'),
          definition = function(object, complete=FALSE, day=FALSE){
              g0 <- copy(object)
              BTi <- indexI(g0)
              BTi <- truncDay(BTi)
              ind.rep <- cumsum(c(1, diff(BTi)!=0))
              G0I <- g0@G0I
              solI <- g0@solI
              solD <- g0@solD[ind.rep]
              Ta <- g0@Ta
              if(length(Ta[[1]]!=length(G0I[[1]]))) Ta <- Ta[ind.rep]
              if(complete){
                  data <- data.table(solI,
                                     G0I[, Dates := NULL],
                                     solD[, Dates := NULL],
                                     Ta[, Dates := NULL])
              } else{    
                  G0I[, Kt := NULL]
                  G0I[, Fd := NULL]
                  data <- G0I
              }
              if(day){
                  ind <- indexI(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableI',
          signature = (object='Gef'),
          definition = function(object, complete=FALSE, day=FALSE){
              gef <- copy(object)
              BTi <- indexI(gef)
              BTi <- truncDay(BTi)
              ind.rep <- cumsum(c(1, diff(BTi, units='days')!=0))
              GefI <- gef@GefI
              G0I <- gef@G0I
              solI <- gef@solI
              solD <- gef@solD[ind.rep]
              Ta <- gef@Ta
              if(length(Ta[[1]]!=length(GefI[[1]]))) Ta <- Ta[ind.rep]
              if(complete){
                  data <- data.table(solI,
                                     G0I[, Dates := NULL],
                                     solD[, Dates := NULL],
                                     Ta[, Dates := NULL],
                                     GefI[, Dates := NULL])
              } else {
                  data <- GefI[, c('Dates','Gef',
                                   'Bef', 'Def')]
              }
              if(day){
                  ind <- indexI(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableI',
          signature = (object='ProdGCPV'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodgcpv <- copy(object)
              BTi <- indexI(prodgcpv)
              BTi <- truncDay(BTi)
              ind.rep <- cumsum(c(1, diff(BTi, units = 'days')!=0))
              prodI <- prodgcpv@prodI
              Theta <- prodgcpv@Theta
              GefI <- prodgcpv@GefI
              G0I <- prodgcpv@G0I
              solI <- prodgcpv@solI
              solD <- prodgcpv@solD[ind.rep]
              Ta <- prodgcpv@Ta
              if(length(Ta[[1]]!=length(prodI[[1]]))) Ta <- Ta[ind.rep]
              if(complete){
                  data <- data.table(solI,
                                     G0I[, Dates := NULL],
                                     solD[, Dates := NULL],
                                     Ta[, Dates := NULL],
                                     GefI[, Dates := NULL],
                                     prodI[, Dates := NULL],
                                     Theta[, Dates := NULL])
              } else {
                  data <- prodI[, c('Dates', 'Pac', 'Pdc')]
              }
              if(day){
                  ind <- indexI(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableI',
          signature = (object='ProdPVPS'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodpvps <- copy(object)
              BTi <- indexI(prodpvps)
              BTi <- truncDay(BTi)
              ind.rep <- cumsum(c(1, diff(BTi, units='days')!=0))
              prodI <- prodpvps@prodI
              Theta <- prodpvps@Theta
              GefI <- prodpvps@GefI
              G0I <- prodpvps@G0I
              solI <- prodpvps@solI
              solD <- prodpvps@solD[ind.rep]
              Ta <- prodpvps@Ta
              if(length(Ta[[1]]!=length(prodI[[1]]))) Ta <- Ta[ind.rep]
              if(complete){
                  data <- data.table(solI,
                                     G0I[, Dates := NULL],
                                     solD[, Dates := NULL],
                                     Ta[, Dates := NULL],
                                     GefI[, Dates := NULL],
                                     prodI[, Dates := NULL],
                                     Theta[, Dates := NULL])
              } else {
                  data <- prodI[, c('Dates', 'Pac', 'Pdc')]
              }
              if(day){
                  ind <- indexI(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

### as.data.tableD ###
## Extracts a data.table with daily values ##
setGeneric('as.data.tableD', function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.tableD')})

setMethod('as.data.tableD',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
              sol <- copy(object)
              solD <- sol@solD
              data <- solD
              if(day){
                  ind <- indexD(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableD',
          signature = (object='G0'),
          definition = function(object, complete=FALSE, day=FALSE){
              g0 <- copy(object)
              G0D <- g0@G0D
              solD <- g0@solD
              if(complete){
                  data <- data.table(G0D, solD[, Dates := NULL])
              } else {
                  G0D[, Fd := NULL]
                  G0D[, Kt := NULL]
                  data <- G0D
              }
              if(day){
                  ind <- indexD(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          })

setMethod('as.data.tableD',
          signature = (object='Gef'),
          definition = function(object, complete=FALSE, day=FALSE){
              gef <- copy(object)
              GefD <- gef@GefD
              G0D <- gef@G0D
              solD <- gef@solD
              if(complete){
                  data <- data.table(GefD,
                                     G0D[, Dates := NULL],
                                     solD[, Dates := NULL])
              } else {data <- GefD[, c('Dates', 'Gefd',
                                       'Defd', 'Befd')]}
              if(day){
                  ind <- indexD(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]     
              }
              return(data)
          }
          )

setMethod('as.data.tableD',
          signature = (object='ProdGCPV'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodgcpv <- copy(object)
              prodD <- prodgcpv@prodD
              GefD <- prodgcpv@GefD
              G0D <- prodgcpv@G0D
              solD <- prodgcpv@solD
              if(complete){
                  data <- data.table(prodD,
                                     GefD[, Dates := NULL],
                                     G0D[, Dates := NULL],
                                     solD[, Dates := NULL]
                                     )
              } else { data <- prodD[, c('Dates', 'Eac',
                                         'Edc', 'Yf')]}
              if(day){
                  ind <- indexD(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableD',
          signature = (object='ProdPVPS'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodpvps <- copy(object)
              prodD <- prodpvps@prodD
              GefD <- prodpvps@GefD
              G0D <- prodpvps@G0D
              solD <- prodpvps@solD
              if(complete){
                  data <- data.table(prodD,
                                     GefD[, Dates := NULL],
                                     G0D[, Dates := NULL],
                                     solD[, Dates := NULL]
                                     )
              } else { data <- prodD[, c('Dates', 'Eac',
                                         'Qd', 'Yf')]}
              if(day){
                  ind <- indexD(object)
                  data[, day := doy(ind)]
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

### as.data.tableM ###
## Extracts a data.table with monthly means ##
setGeneric('as.data.tableM', function(object, complete = FALSE, day=FALSE){standardGeneric('as.data.tableM')})

setMethod('as.data.tableM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE, day=FALSE){
              g0 <- copy(object)
              G0dm <- g0@G0dm
              data <- G0dm
              if(day){
                  ind <- indexD(object)
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableM',
          signature=(object='Gef'),
          definition = function(object, complete=FALSE, day=FALSE){
              gef <- copy(object)
              Gefdm <- gef@Gefdm
              G0dm <- gef@G0dm
              if(complete){
                  data <- data.table(Gefdm, G0dm[, Dates := NULL])
              } else {data <- Gefdm}
              if(day){
                  ind <- indexD(object)
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableM',
          signature = (object='ProdGCPV'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodgcpv <- copy(object)
              prodDm <- prodgcpv@prodDm
              Gefdm <- prodgcpv@Gefdm
              G0dm <- prodgcpv@G0dm
              if(complete){
                  data <- data.table(prodDm,
                                     Gefdm[, Dates := NULL],
                                     G0dm[, Dates := NULL])
              } else {data <- prodDm}
              if(day){
                  ind <- indexD(object)
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

setMethod('as.data.tableM',
          signature = (object='ProdPVPS'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodpvps <- copy(object)
              prodDm <- prodpvps@prodDm
              Gefdm <- prodpvps@Gefdm
              G0dm <- prodpvps@G0dm
              if(complete){
                  data <- data.table(prodDm,
                                     Gefdm[, Dates := NULL],
                                     G0dm[, Dates := NULL])
              } else {data <- prodDm}
              if(day){
                  ind <- indexD(object)
                  data[, month := month(ind)]
                  data[, year := year(ind)]
              }
              return(data)
          }
          )

### as.data.frameY ###
## Extracts a data.table with yearly values ##
setGeneric('as.data.tableY', function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.tableY')})

setMethod('as.data.tableY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE, day=FALSE){
              g0 <- copy(object)
              G0y <- g0@G0y
              data <- G0y
              if(day){data[, year := Dates]}
              return(data)
          }
          )

setMethod('as.data.tableY',
          signature = (object='Gef'),
          definition = function(object, complete=FALSE, day=FALSE){
              gef <- copy(object)
              Gefy <- gef@Gefy
              G0y <- gef@G0y
              if(complete){
                  data <- data.table(Gefy, G0y[, Dates := NULL])
              } else {data <- Gefy}
              if(day){data[, year := Dates]}
              return(data)
          }
          )

setMethod('as.data.tableY',
          signature = (object='ProdGCPV'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodgcpv <- copy(object)
              prody <- prodgcpv@prody
              Gefy <- prodgcpv@Gefy
              G0y <- prodgcpv@G0y
              if(complete){
                  data <- data.table(prody,
                                     Gefy[, Dates := NULL],
                                     G0y[, Dates := NULL])       
              } else {data <- prody}
              if(day){data[, year := Dates]}
              return(data)
          }
          )

setMethod('as.data.tableY',
          signature = (object='ProdPVPS'),
          definition = function(object, complete=FALSE, day=FALSE){
              prodpvps <- copy(object)
              prody <- prodpvps@prody
              Gefy <- prodpvps@Gefy
              G0y <- prodpvps@G0y
              if(complete){
                  data <- data.table(prody,
                                     Gefy[, Dates := NULL],
                                     G0y[, Dates := NULL])       
              } else {data <- prody}
              if(day){data[, year := Dates]}
              return(data)
          }
          )
