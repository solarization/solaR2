\name{C_solarAngles}
\alias{declination}
\alias{eccentricity}
\alias{eot}
\alias{sunrise}
\alias{bo0d}
\alias{sunHour}
\alias{zenith}
\alias{azimuth}
\title{Solar angles}
\description{A set of functions that compute the apparent movement of
  the Sun from the Earth.}
\usage{
## Declination
declination(d, method = 'michalsky')

## Eccentricity
eccentricity(d, method = 'michalsky')

## Equation of time
eot(d)

## Solar time
sunrise(d, lat, method = 'michalsky',
        decl = declination(d, method))


## Extraterrestrial irradiation
bo0d(d, lat, method = 'michalsky',
     decl = declination(d, method),
     eo = eccentricity(d, method),
     ws = sunrise(d, lat, method))

## Sun hour angle
sunHour(d, BTi, sample = 'hour', EoT = TRUE,
        method = 'michalsky',
        eqtime = eot(d))

## Cosine of the zenith angle
zenith(d, lat, BTi, sample = 'hour', method = 'michalsky',
       decl = declination(d, method),
       w = sunHour(d, BTi, sample, method = method))

## Azimuth angle
azimuth(d, lat, BTi, sample = 'hour', method = 'michalsky',
        decl = declination(d, method),
        w = sunHour(d, BTi, sample, method),
        cosThzS = zenith(d, lat, BTi, sample, method, decl, w))
}

\arguments{
  \item{d}{Date, a daily time base, it may be the result of
    \code{\link{fBTd}}}

  \item{method}{character, method for the sun geometry calculations, to
    be chosen from 'cooper', 'spencer', 'michalsky' and 'strous'. See
    references for details.}

  \item{lat}{numeric, latitude (degrees) of the point of the Earth where
    calculations are needed.
    It is positive for locations above the Equator.}

  \item{sample}{Character, increment of the intradaily sequence.}

  \item{BTi}{\code{POSIXct}, intradily time base, it may the result of
    \code{\link{fBTi}}.}
  
}

\references{\itemize{ \item{Cooper, P.I., Solar Energy, 12, 3
    (1969). "The Absorption of Solar Radiation in Solar Stills"}
    \item{Spencer, Search 2 (5), 172,
    \url{https://www.mail-archive.com/sundial@uni-koeln.de/msg01050.html}}
    \item{Strous:
    \url{https://www.aa.quae.nl/en/reken/zonpositie.html}}
    \item{Michalsky, J., 1988: The Astronomical Almanac's algorithm for
    approximate solar position (1950-2050), Solar Energy 40, 227-235}
  \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
    Systems with R", Journal of Statistical Software, 50(9), 1-32,
    \doi{10.18637/jss.v050.i09} } }

\seealso{
  \code{\link{fSolD}},
  \code{\link{fSolI}},
  \code{\link{calcSol}}
}

\examples{

d = fBTd(mode = 'serie')[100]

decl = declination(d, method = 'michalsky')
decl

w = sunHour(d, sample = 'hour', method = 'michalsky')
w

cosThzS = zenith(d, lat = 37.2, sample = 'hour',
                 method = 'michalsky',
                 decl = decl,
                 w = w)
cosThzS

}

\keyword{utilities}
