DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31)

setMethod('[',
          signature='Meteo',
          definition=function(x, i, j,...){
            if (!missing(i)) {
              i <- truncDay(i)
            } else {
              i <- indexD(x)[1]
            }
            if (!missing(j)) {
              j <- truncDay(j)+86400-1 ##The end is the last second of the day
            } else {
              nDays <- length(indexD(x))
              j <- indexD(x)[nDays]+86400-1
            }
            stopifnot(j>i)
            if (!is.null(i)) i <- truncDay(i)
            if (!is.null(j)) j <- truncDay(j)+86400-1
            ## d <- as.POSIXct(indexD(x))
            d <- indexD(x)
            x@data <- x@data[(d >= i & d <= j)]
            x
          }
          )


setMethod('[',
          signature='Sol',
          definition=function(x, i, j, ...){
              if (!missing(i)) {
                  i <- truncDay(i)
              } else {
                  i <- indexD(x)[1]
              }
              if (!missing(j)) {
                  j <- truncDay(j)+86400-1##The end is the last second of the day
              } else {
                  nDays <- length(indexD(x))
                  j <- indexD(x)[nDays]+86400-1
              }
              stopifnot(j>i)
              if(!is.null(i)) i <- truncDay(i)
              if(!is.null(j)) j <- truncDay(j)
              d1 <- indexD(x)
              d2 <- indexI(x)
              x@solD <- x@solD[(d1 >= i & d1 <= j)]
              x@solI <- x@solI[(d2 >= i & d2 <= j)]
              x
          }
          )

setMethod('[',
          signature='G0',
          definition=function(x, i, j, ...){
              sol <- as(x, 'Sol')[i=i, j=j, ...] ##Sol method
              meteo <- as(x, 'Meteo')[i=i, j=j, ...] ##Meteo method
              i <- indexI(sol)[1]
              j <- indexI(sol)[length(indexI(sol))]
              d1 <- indexD(x)
              d2 <- indexI(x)
              ## G0Iw <- window(x@G0I, start=start, end=end)
              G0Iw <- x@G0I[(d2 >= i & d2 <= j)]
              ## Taw <- window(x@Ta, start=start, end=end)
              Taw <- x@Ta[(d2 >= i & d2 <= j)]
              ## G0dw <- window(x@G0D, start=truncDay(start), end=truncDay(end))
              G0dw <- x@G0D[(d1 >= truncDay(i) & d1 <= truncDay(j))]
              ## G0dmw <- aggregate(G0dw[,c('G0d', 'D0d', 'B0d')], by=as.yearmon,
              ##                    FUN=function(x, ...)mean(x, na.rm=1)/1000) ##kWh
              G0dmw <- G0dw[, lapply(.SD/1000, mean, na.rm= TRUE),
                            .SDcols = c('G0d', 'D0d', 'B0d'),
                            by = .(month(Dates), year(Dates))]
              if (x@type=='prom'){
                  ## G0yw=zoo(t(colSums(G0dmw*DayOfMonth)),
                  ##          unique(year(index(G0dmw))))
                  G0dmw[, DayOfMonth := DOM(G0dmw)]
                  G0yw <- G0dmw[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                                .SDcols = c('G0d', 'D0d', 'B0d'),
                                by = year]
              } else {
                  ## G0yw=aggregate(G0dw[,c('G0d', 'D0d', 'B0d')], by=year,
                  ##                FUN=function(x, ...)sum(x, na.rm=1)/1000) ##kWh
                  G0yw <- G0dw[, lapply(.SD/1000, sum, na.rm = TRUE),
                               .SDcols = c('G0d', 'D0d', 'B0d'),
                               by = year(unique(truncDay(Dates)))]
              }
              result <- new('G0',
                            meteo,
                            sol,
                            G0D=G0dw,
                            G0dm=G0dmw,
                            G0y=G0yw,
                            G0I=G0Iw,
                            Ta=Taw)
              result
          }
          )


setMethod('[',
          signature='Gef',
          definition=function(x, i, j, ...){
            g0 <- as(x, 'G0')[i=i, j=j, ...] ##G0 method

            ## The sol methods already includes a procedure to correct the start and end values
            idx <- indexI(g0)
            start <- idx[1]
            end <- idx[length(idx)]


            GefIw <- window(x@GefI, start=start, end=end,...) ##zoo method
            Thetaw <- window(x@Theta, start=start, end=end,...) ##zoo method
            Gefdw <- window(x@GefD, start=truncDay(start), end=truncDay(end), ...) ##zoo method

            Gefdmw <- aggregate(Gefdw[,c('Bod', 'Bnd', 'Gd', 'Dd', 'Bd', 'Gefd', 'Defd', 'Befd')],
                                by=as.yearmon,
                                FUN=function(x, ...)mean(x, na.rm=1)/1000) ##kWh
            if (x@type=='prom'){
              Gefyw=zoo(t(colSums(Gefdmw*DayOfMonth)),
                unique(year(index(Gefdmw))))
            } else {
              Gefyw=aggregate(Gefdw[,c('Bod', 'Bnd', 'Gd', 'Dd', 'Bd', 'Gefd', 'Defd', 'Befd')],
                by=year,
                FUN=function(x, ...)sum(x, na.rm=1)/1000) ##kWh
            }

            result <- new('Gef',
                          g0,
                          GefD=Gefdw,
                          Gefdm=Gefdmw,
                          Gefy=Gefyw,
                          GefI=GefIw,
                          Theta=Thetaw,
                          iS=x@iS,
                          alb=x@alb,
                          modeTrk=x@modeTrk,
                          modeShd=x@modeShd,
                          angGen=x@angGen,
                          struct=x@struct,
                          distances=x@distances
                          )
            result
          }
          )


setMethod('[',
          signature='ProdGCPV',
          definition=function(x, i, j, ...){
            gef <- as(x, 'Gef')[i=i, j=j, ...] ##Gef method

            ## The sol methods already includes a procedure to correct the start and end values
            idx <- indexI(gef)
            start <- idx[1]
            end <- idx[length(idx)]


            prodIw <- window(x@prodI, start=start, end=end,...) ##zoo method
            prodDw <- window(x@prodD, start=truncDay(start), end=truncDay(end),...) ##zoo method

            if (x@type=='prom'){
              prodDmw <- prodDw/1000
              prodDmw$Yf <- prodDw$Yf
              prodyw=zoo(t(colSums(prodDmw*DayOfMonth)),
                unique(year(index(prodDmw))))
            } else {
              prodDmw <- aggregate(prodDw/1000,
                                   by=as.yearmon,
                                   mean, na.rm=1)
              prodyw=aggregate(prodDw/1000,
                by=year,
                sum, na.rm=1) ##kWh
              prodDmw$Yf=prodDmw$Yf*1000
              prodyw$Yf=prodyw$Yf*1000
            }

            result <- new('ProdGCPV',
                          gef,
                          prodD=prodDw,
                          prodDm=prodDmw,
                          prody=prodyw,
                          prodI=prodIw,
                          module=x@module,
                          generator=x@generator,
                          inverter=x@inverter,
                          effSys=x@effSys
                          )
            result
          }
          )

setMethod('[',
          signature='ProdPVPS',
          definition=function(x, i, j, ...){
            gef <- as(x, 'Gef')[i=i, j=j, ...] ##Gef method

            ## The sol methods already includes a procedure to correct the start and end values
            idx <- indexI(gef)
            start <- idx[1]
            end <- idx[length(idx)]


            prodIw <- window(x@prodI, start=start, end=end,...) ##zoo method
            prodDw <- window(x@prodD, start=truncDay(start), end=truncDay(end),...) ##zoo method

            if (x@type=='prom'){
              prodDmw <- prodDw
              prodDmw$Eac <- prodDw$Eac/1000
              prodyw=zoo(t(colSums(prodDmw*DayOfMonth)),
                unique(year(index(prodDmw))))
            } else {
              prodDmw <- aggregate(prodDw,
                                   by=as.yearmon,
                                   mean, na.rm=1)
              prodyw=aggregate(prodDw,
                by=year,
                sum, na.rm=1) ##kWh
              prodDmw$Eac=prodDmw$Eac/1000
              prodyw$Eac=prodyw$Eac/1000
            }

            result <- new('ProdPVPS',
                          gef,
                          prodD=prodDw,
                          prodDm=prodDmw,
                          prody=prodyw,
                          prodI=prodIw,
                          pump=x@pump,
                          H=x@H,
                          Pg=x@Pg,
                          converter=x@converter,
                          effSys=x@effSys
                          )
            result
          }
          )
