\name{A6_calcShd}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{calcShd}

\title{Shadows on PV systems.}

\description{ Compute the irradiance and irradiation including shadows
  for two-axis and horizontal N-S axis trackers and fixed surfaces.  It
  makes use of the function \code{\link{fSombra}} for the shadows factor
  calculation. It is used by the function \code{\link{calcGef}}.  }

\usage{

calcShd(radEf,
        modeShd = '',
        struct = list(),
        distances = data.table())
}

\arguments{ \item{radEf}{A
  \code{\linkS4class{Gef}} object. It may be the result of the
  \code{\link{calcGef}} function.}

  \item{modeShd}{character, defines the type of shadow calculation. In
    this version of the package the effect of the shadow is calculated
    as a proportional reduction of the circumsolar diffuse and direct
    irradiances. This type of approach is selected with
    \code{modeShd = 'area'}. In future versions other approaches which
    relate the geometric shadow and the electrical connections of the PV
    generator will be available. If \code{radEf@modeTrk = 'horiz'} it is
    possible to calculate the effect of backtracking with
    \code{modeShd = 'bt'}. If \code{modeShd = c('area','bt')} the
    backtracking method will be carried out and therefore no shadows
    will appear. Finally, for two-axis trackers it is possible to select
    \code{modeShd = 'prom'} in order to calculate the effect of shadows on
    an average tracker (see \code{\link{fSombra6}}).  The result will
    include three variables (Gef0, Def0 and Bef0) with the
    irradiance/irradiation without shadows as a reference.}


  \item{struct}{\code{list}.

    When \code{radEf@modeTrk = 'fixed'} or \code{modeTrk = 'horiz'} only a
    component named \code{L}, which is the height (meters) of the
    tracker, is needed.

    For two-axis trackers (\code{radEf@modeTrk = 'two'}), an additional
    component named \code{W}, the width of the tracker, is
    required. Moreover, only when \code{radEf@modeTrk = 'two'} two components named
    \code{Nrow} and \code{Ncol} are included under this list. These
    components define, respectively, the number of rows and columns of
    the whole set of two-axis trackers in the PV plant.}

  \item{distances}{\code{data.frame}.

    When \code{radEf@modeTrk = 'fixed'} it includes a component named \code{D}
    for the distance between fixed surfaces. An additional component
    named \code{H} can be included with the relative height between
    surfaces.

    When \code{radEf@modeTrk = 'horiz'} it only includes a component named
    \code{Lew}, being the distance between horizontal NS trackers along
    the East-West direction.

    When \code{radEf@modeTrk = 'two'} it includes a component named \code{Lns}
    being the distance between trackers along the North-South direction,
    a component named \code{Lew}, being the distance between trackers
    along the East-West direction and a (optional) component named
    \code{H} with the relative height between surfaces.

  The distances, in meters, are defined between axis of the trackers.}
}

\value{A \code{Gef} object including three additional variables
  (\code{Gef0}, \code{Def0} and \code{Bef0}) in the slots \code{GefI},
  \code{GefD}, \code{Gefdm} and \code{Gefy} with the
  irradiance/irradiation without shadows as a reference.}

\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
    Systems with R", Journal of Statistical Software, 50(9), 1-32,
    \doi{10.18637/jss.v050.i09}
  }}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}

\seealso{ \code{\link{calcG0}}, \code{\link{fTheta}},
  \code{\link{fInclin}}, \code{\link{calcShd}}.  }


\keyword{constructors}
