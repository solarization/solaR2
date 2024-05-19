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
                  sun2 <- object@solD
                  sun <- data.table(sun, sun2[, -'Dates'])
              }
              return(sun)
          }
          )

###as.data.frameD
setGeneric('as.data.tableD', function(object){standardGeneric('as.data.tableD')})

setMethod('as.data.tableD',
          signature=(object='Sol'),
          definition=function(object){
            zoo0=as.zooD(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$day=doy(ind)##Incorporo dia, mes y año como columnas del data.frame
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameM
setGeneric('as.data.frameM', function(object, complete=FALSE){standardGeneric('as.data.frameM')})

setMethod('as.data.frameM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooM(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
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
