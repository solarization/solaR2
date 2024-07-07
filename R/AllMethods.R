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

## convert daily values ​​to the same length as intradaily values ##
setGeneric('indexRep', function(object){standardGeneric('indexRep')})

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
          definition = function(object){object@solD$Dates
          })


### indexI ###
setMethod('indexI',
          signature = (object = 'Sol'),
          definition = function(object){object@solI$Dates
          })

### indexRep###
setMethod('indexRep',
          signature=(object='Sol'),
          definition=function(object){
              #########
              x <- as.Date(indexI(object))
              cumsum(c(1, diff(x) != 0))
          }
          )

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
          definition = function(object){object@data$Dates
          })

### indexI ###
setMethod('indexI',
          signature = (object = 'Meteo'),
          definition = function(object){object@data$Dates
          })

#### Methods for G0 ####

### getG0 ###
setMethod('getG0',
          signature = (object = 'Meteo'),
          definition = function(object){
              result <- getData(object)
              return(result$G0)
          })

### show ###
setMethod('show',
          signature = (object = 'G0'),
          definition = function(object){
              cat('Object of class ', class(object),'\n\n')
              cat('Source of meteorological information: ')
              cat(paste(object@type, object@source, sep='-'),'\n\n')
              cat('Latitude of source: ',
                  paste(round(getLat(as(object, 'Meteo'),'deg'), 1),
                        'degrees\n'))
              cat('Latitude for calculations: ',
                  paste(round(getLat(object, 'deg'),1), 'degrees\n\n'))
              cat('Monthly avarages:\n')
              print(as.data.tableM(object))
              cat('\nYearly values:\n')
              print(as.data.tableY(object))
          })

#### Methods for Gef ####

### show ###
setMethod('show',
          signature = (object = 'Gef'),
          definition = function(object){
              callNextMethod()
              cat('-----------------\n')
              cat('Mode of tracking: ', object@modeTrk,'\n')
              if (object@modeTrk=='fixed'){
                  cat('    Inclination: ', object@angGen$beta, '\n')
                  cat('    Orientation: ', object@angGen$alfa, '\n')
              } else {
                  cat('    Inclination limit:', object@angGen$betaLim, '\n')
              }
          })

#### Methods for prodGCPV ####
