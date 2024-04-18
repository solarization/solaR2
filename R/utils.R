#### Angles ####
#degrees to radians
d2r<-function(x){x*pi/180}

#radians to degrees
r2d<-function(x){x*180/pi}

#hours to radians
h2r<-function(x){x*pi/12}

#hours to degrees
h2d<-function(x){x*180/12}

#radians to hours
r2h<-function(x){x*12/pi}

#degrees to hours
d2h<-function(x){x*12/180}

#radians to seconds
r2sec<-function(x){x*12/pi*3600}

#radians to minutes
r2min<-function(x){x*12/pi*60}

#### Time ###
#hour
hour <- function(x) 
{
    as.numeric(format(x, "%H"))
}

#minute
minute <- function(x)
{
    as.numeric(format(x, "%M"))
}

#second
second <- function(x) 
{
    as.numeric(format(x, "%S"))
}

#complete time to hours
t2h <- function(x)
{
    hour(x)+minute(x)/60+second(x)/3600
}

#day of the year
doy <- function(x){
  as.numeric(format(x, '%j'))
}

#day of the month
dom <- function(x){
  as.numeric(format(x, '%d'))
}

#month
month <- function(x){
  as.numeric(format(x, '%m'))
}

#year
year <- function(x){
  as.numeric(format(x, '%Y'))
}

#trunc days
truncDay <- function(x){as.POSIXct(trunc(x, units='days'))}

## Check if daily indexes are equal (used in fCompD and fTemp)
checkIndexD <- function(ix, iy)
{
    dx <- truncDay(ix)
    dy <- truncDay(iy)
    test <- all.equal(dx, dy,  check.attributes = FALSE)
    if (!isTRUE(test))
        stop('daily indexes do not match.')
}

##difftime to hours
diff2Hours  <- function(by){
  if (!inherits(by, 'difftime')) {
    stop('This function is only useful for difftime objects.')
  } else {
    return(as.numeric(by, units='hours'))
  }
}

# character to difftime
char2diff <- function(by){
  if (!is.character(by)) {
    stop('This function is only useful for character strings.')
  } else {
    ##Adaptado de seq.POSIXt
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L) 
      stop("invalid 'by' string")
    units <- c("secs", "mins", "hours")
    valid <- pmatch(by2[length(by2)], units)
    if (is.na(valid)) {
      stop("invalid string for 'by'")
    } else {
      unitValid <- units[valid]
      if (length(by2)==1) {
        by2=1
      } else {
        by2=as.numeric(by2[1])
      }
      result <- as.difftime(by2,units=unitValid)
      return(result)
    }
  }
}

# sample to hours
sample2Hours <- function(by){
  if (is.character(by)) {
    y <- char2diff(by)
    return(diff2Hours(y))
  } else if (inherits(by, 'difftime')) {
    return(diff2Hours(by))
  } else {stop('by must be a character or difftime.')}
}
  

#### Energy ####
# Power to energy
P2E <- function(x, by){
    Nm <- 1/sample2Hours(by)
    sum(x, na.rm = 1)/Nm
}
