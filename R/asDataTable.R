setGeneric('as.data.table')

###as.data.frameI
setGeneric('as.data.tableI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.tableI')})

setMethod('as.data.tableI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
              sun <- object@solI
              if(day){
                  sun[, day := doy(Dates)]
              }
              if(complete){
                  sun2 <- as.data.tableD(object)
                  sun <- data.table(sun, sun2[, -'Dates'])
              }
              return(sun)
          }
          )

###as.data.frameD
setGeneric('as.data.tableD', function(object, day = FALSE){standardGeneric('as.data.tableD')})

setMethod('as.data.tableD',
          signature=(object='Sol'),
          definition=function(object, day = FALSE){
              sun <- object@solD
              if(day){
                  d <- indexD(object)
                  sun[, day := doy(d)]
                  sun[, month :=  month(d)]
                  sun[, year := year(d)]
              }
              return(sun)
          }
          )

###as.data.frameM
setGeneric('as.data.frameM', function(object){standardGeneric('as.data.frameM')})

setMethod('as.data.frameM',
          signature=(object='G0'),
          definition=function(object, day = FALSE){
              g0 <- object@G0dm
              if(day){
                  
              }
              return(g0)
          }
          )

###as.data.frameY
setGeneric('as.data.frameY', function(object, complete=FALSE){standardGeneric('as.data.frameY')})

setMethod('as.data.frameY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooY(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$year=ind
            return(data0)
          }
          )
