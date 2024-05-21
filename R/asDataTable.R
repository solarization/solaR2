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
                  sun$day <- doy(d)
                  sun$month <- month(d)
                  sun$year <- year(d)
              }
              return(sun)
          }
          )

###as.data.frameM
setGeneric('as.data.tableM', function(object){standardGeneric('as.data.tableM')})

setMethod('as.data.tableM',
          signature=(object='G0'),
          definition=function(object){
              return(object@G0dm)
          }
          )

###as.data.frameY
setGeneric('as.data.tableY', function(object){standardGeneric('as.data.tableY')})

setMethod('as.data.tableY',
          signature=(object='G0'),
          definition=function(object){
              return(object@G0y)
          }
          )
