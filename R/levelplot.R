setGeneric('levelplot')

## setMethod('levelplot',
##           signature=c(x='formula', data='zoo'),
##           definition=function(x, data,
##             par.settings=solaR.theme,
##             ##            panel=panel.levelplot.raster, interpolate=TRUE,...){
##             xscale.components=xscale.solar,
##             yscale.components=yscale.solar,
##             ...){
##             data0=as.data.frame(data)
##             ind=index(data)
##             data0$day=doy(ind) ##Incorporo dia, mes y año para facilitar la formula.
##             data0$month=month(ind)
##             data0$year=year(ind)
##             if (!('w' %in% names(data0))){
##               data0$w=h2r(hms(ind)-12) ##hora solar en radianes
##             }
##             levelplot(x, data0, par.settings=par.settings,
##                       xscale.components=xscale.components,
##                       yscale.components=yscale.components,
##                       ##                     panel=panel, interpolate=interpolate,
##                       ...)
##           }
##           )


setMethod('levelplot',
          signature=c(x='formula', data='Meteo'),
          definition=function(x, data,
                              par.settings = solaR.theme,
                              panel = panel.levelplot.raster, interpolate = TRUE,
                              xscale.components = xscale.solar,
                              yscale.components = yscale.solar,
                              ...){
            data0=getData(data)
            levelplot(x, data0,
                      par.settings = par.settings,
                      xscale.components = xscale.components,
                      yscale.components = yscale.components,
                      panel = panel, interpolate = interpolate,
                      ...)
          }
          )

setMethod('levelplot',
          signature=c(x='formula', data='Sol'),
          definition=function(x, data,
                              par.settings = solaR.theme,
                              panel = panel.levelplot.raster, interpolate = TRUE,
                              xscale.components = xscale.solar,
                              yscale.components = yscale.solar,
                              ...){
            data0=as.data.tableI(data, complete=TRUE, day=TRUE)
            levelplot(x, data0,
                      par.settings = par.settings,
                      xscale.components = xscale.components,
                      yscale.components = yscale.components,
                      panel = panel, interpolate = interpolate,
                      ...)
          }
          )

setMethod('levelplot',
          signature=c(x='formula', data='G0'),
          definition=function(x, data,
                              par.settings = solaR.theme,
                              panel = panel.levelplot.raster, interpolate = TRUE,
                              xscale.components = xscale.solar,
                              yscale.components = yscale.solar,
                              ...){
            data0=as.data.tableI(data, complete=TRUE, day=TRUE)
            levelplot(x, data0, 
                      par.settings = par.settings,
                      xscale.components = xscale.components,
                      yscale.components = yscale.components,
                      panel = panel, interpolate = interpolate,
                      ...)
          }
          )
