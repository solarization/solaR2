.solEnv <- new.env()
assign('oldTZ', Sys.getenv('TZ'), envir = .solEnv)

.onLoad <- function(libpath, pkgname)
{
    Sys.setenv(TZ='UTC')
}

.onAttach <- function(libpath, pkgname){
    packageStartupMessage('Time Zone set to UTC.\n')
}
    
.onUnload <- function(libpath)
{
    oldTZ <- get('oldTZ', envir = .solEnv)
    Sys.setenv(TZ = oldTZ)
}


## if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "Dates", "Bo0d",
##                                                         "Bo0m", "G0d", "DayOfMonth",
##                                                         "Bo0", "Ta", "lat", "decl",
##                                                         "eo", "EoT", "ws", "eqtime",
##                                                         "w", "cosThzS", "night",
##                                                         "AlS", "AzS", "Times",
##                                                         "Lew", "Lns", "H", "Yf",
##                                                         "Eac", "..cols", "Codigo",
##                                                         "EffI", "Estacion", "Fd",
##                                                         "Fecha", "Fecha_Baja",
##                                                         "Fecha_Instalacion",
##                                                         "HoraMin", "TempMin",
##                                                         "Vmpp", "Year", "est_SIAR",
##                                                         "index", "peso",
##                                                         "req_perform",
##                                                         "req_url_path",
##                                                         "req_url_query",
##                                                         "request", "resp_body_json",
##                                                         "variable", "Impp",
##                                                         "Kt", "Latitud",
##                                                         "Longitud", "Mes", "Pdc",
##                                                         "Qd", "Radiacion",
##                                                         "TempMax", "TempMedia"))
