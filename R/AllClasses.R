setOldClass('zoo')
setOldClass('loess')
setOldClass('difftime')
setOldClass('data.table')

#### Sol class ####
setClass(
         Class='Sol', ##Solar angles
         slots = c(
             lat='numeric',#latitud in degrees, >0 if North
             solD='data.table',#daily angles
             solI='data.table',#intradaily angles
             sample='character',#sample of time
             method='character'#method used for geometry calculations
         ),
    validity=function(object) {return(TRUE)}
)

#### Meteo class ####
setClass(
    Class = 'Meteo', ##radiation and temperature data
    slots = c(
        lat='numeric',#latitud in degrees, >0 if North
        data='data.table',#data, incluying G (Wh/m2) and Ta(ºC)
        type='character',#choose between 'prom', 'bd' and 'bdI'
        source='character'#origin of the data
    ),
    validity=function(object) {return(TRUE)}
)

#### G0 class ####
setClass(
    Class = 'G0',
    slots = c(
        G0D = 'data.table',
        G0dm = 'data.table',
        G0y = 'data.table',
        G0I = 'data.table',
        Ta = 'data.table'
    ),
    contains = c('Sol', 'Meteo'),
    validity = function(object) {return(TRUE)}
)

#### Gef class ####
setClass(
         Class='Gef',
         slots = c(
           GefD='data.table',       #aggregate, valores diarios
           Gefdm='data.table',      #aggregate, medias mensuales
           Gefy='data.table',       #aggregate, valores anuales
           GefI='data.table',       #resultado de fInclin
           Theta='data.table',     #resultado de fTheta
           iS='numeric',     #indice de suciedad OJO ¿pasar a INTEGER?
           alb='numeric',    #albedo
           modeTrk='character',         #modo de seguimiento
           modeShd='character',         #modo de sombra
           angGen='list',               # incluye alfa, beta y betaLim
           struct='list',               #dimensiones de la estructura
           distances='data.frame'       #distancias entre estructuras
           ),
         contains='G0',
         validity=function(object) {return(TRUE)}
         )

#### ProdGCPV class ####
setClass(
         Class='ProdGCPV',
         slots = c(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           module='list',
           generator='list',
           inverter='list',
           effSys='list'
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

#### ProdPVPS class ####
setClass(
         Class='ProdPVPS',
         slots = c(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           Pg='numeric',
           H='numeric',
           pump='list',
           converter='list',
           effSys='list'
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

#### Shade class ####
setClass(
         Class='Shade',
         slots = c(
           FS='numeric',
           GRR='numeric',
           Yf='numeric',
           FS.loess='loess',
           Yf.loess='loess',
           modeShd='character',
           struct='list',
           distances='data.frame',
           res='numeric'
           ),
         contains='ProdGCPV',##Resultado de prodGCPV sin sombras (Prod0)
         validity=function(object) {return(TRUE)}
         )
