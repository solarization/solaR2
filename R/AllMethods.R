#### Methods ####
### get ###
## extracts the latitude from the objects ##
setGeneric('getLat', function(object, units = 'rad')
{standardGeneric('getLat')})

## extracts the data for class Meteo ##
setGeneric('getData', function(object){standardGeneric('getData')})

## extracts the global irradiance for class Meteo ##
setGeneric('getG0', function(object){standardGeneric('getG0')})

### index ###
## extract the index of the daily data ##
setGeneric('indexD', function(object){standardGeneric('indexD')})

## extract the index of the intradaily data ##
setGeneric('indexI', function(object){standardGeneric('indexI')})


#### Methods for Sol ####
### getLat ###
setMethod('getLat',
          signature = (object = 'Sol'),
          definition = function(object, units = 'rad'){
              stopifnot(units %in% c('deg', 'rad'))
              result = switch(units,
                           rad = d2r(object@lat),
                           deg = object@lat)
              return(result)
          })

### show ###
setMethod('show',
          signature = (object = 'Sol'),
          definition = function(object){
              cat('Object of class Sol \n\n')
              cat('Latitude: ',
                  paste(round(getLat(object, 'deg'), 1), 'degrees\n\n'))
              cat('Daily values:\n')
              print(summary(object@solD))
              cat('\nIntradaily values: \n')
              print(summary(object@solI))
          })

### indexD ###
setMethod('indexD',
          signature = (object = 'Sol'),
          definition = function(object){object@solD[, .(Dates)]
          })


### indexI ###
setMethod('indexI',
          signature = (object = 'Sol'),
          definition = function(object){object@solI[, .(Dates)]
          })


#### Methods for Meteo ####
### getData ####
setMethod('getData',
          signature = (object = 'Meteo'),
          definition = function(object){
              result <- object@data
              return(result)
          })

### getLat ###
setMethod('getLat',
          signature = (object = 'Meteo'),
          definition = function(object, units = 'rad'){
              stopifnot(units %in% c('deg', 'rad'))
              result = switch(units,
                           rad = d2r(object@lat),
                           deg = object@lat)
              return(result)
          })

### show ###
setMethod('show', 'Meteo',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Source of meteorological information: ')
            cat(paste(object@type, object@source, sep='-'),'\n')
            cat('Latitude of source: ',
                paste(round(getLat(object,'deg'), 1), 'degrees\n\n'))
            cat('Meteorological Data:\n')
            print(summary(getData(object)))
          }
          )

### indexD ###
setMethod('indexD',
          signature = (object = 'Meteo'),
          definition = function(object){object@data[, .(Dates)]
          })

### indexI ###
setMethod('indexI',
          signature = (object = 'Meteo'),
          definition = function(object){object@data[, .(Dates)]
          })

#### Methods for G0 ####

### getG0 ###
setMethod('getG0',
          signature = (object = 'Meteo'),
          definition = function(object){
              result <- getData(object)
              return(result$G0)
          })

### getLat ###
setMethod('getLat',
          signature=(object='G0'),
          definition=function(object, units='rad'){
            getLat(as(object, 'Sol'), units=units)
          }
          )

setMethod('indexD',
          signature=(object='G0'),
          definition=function(object){
            indexD(as(object, 'Sol'))
          }
          )
