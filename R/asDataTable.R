###as.data.tableI
setGeneric('as.data.tableI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.tableI')})

setMethod('as.data.tableI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
              sol <- copy(object)
              solI <- sol@solI
              solD <- sol@solD
              if(complete){
                  data <- data.table(solI, solD[, Dates := NULL])
              } else{data <- solI}
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
          signature = (object='G0'),
          definition = function(object, complete=FALSE, day=FALSE){
              
          }
          )

###as.data.frameD
setGeneric('as.data.tableD', function(object, day = FALSE){standardGeneric('as.data.tableD')})

setMethod('as.data.tableD',
          signature=(object='Sol'),
          definition=function(object, day = FALSE){
              zoo0=as.zooD(object, complete=complete)
              data0=as.data.table(zoo0)
              ind=index(zoo0)
              ##Incorporo dia, mes y año como columnas del data.table
              data0[, day := doy(ind)]
              data0[, month := month(ind)]
              data0[, year := year(ind)]
              return(data0)
          }
          )

###as.data.frameM
setGeneric('as.data.tableM', function(object, complete = FALSE){standardGeneric('as.data.tableM')})

setMethod('as.data.tableM',
          signature=(object='G0'),
          definition=function(object, complete = FALSE){
              zoo0=as.zooM(object, complete = complete)
              data0=as.data.table(zoo0)
              ind=index(zoo0)
              ##Incorporo mes y año como columnas del data.table
              data0[, month := month(ind)]
              data0[, year := year(ind)]
              return(data0)
          }
          )

###as.data.frameY
setGeneric('as.data.tableY', function(object, complete = FALSE){standardGeneric('as.data.tableY')})

setMethod('as.data.tableY',
          signature=(object='G0'),
          definition=function(object, complete = FALSE){
              zoo0=as.zooY(object, complete = complete)
              data0=as.data.table(zoo0)
              ind=index(zoo0)
              ##Incorporo año como columna del data.table
              data0[, year := ind]
              return(data0)
          }
          )
