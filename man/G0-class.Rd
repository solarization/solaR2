\name{B3_G0-class}
\Rdversion{1.1}
\encoding{UTF-8}
\docType{class}
\alias{G0-class}
\alias{show,G0-method}



\title{Class "G0": irradiation and irradiance on the horizontal plane.}
\description{This class contains the global, diffuse and direct
  irradiation and irradiance on the horizontal plane, and ambient
  temperature. }
\section{Objects from the Class}{
  Objects can be created by the function \code{\link{calcG0}}.
}
\section{Slots}{
  \describe{
    \item{\code{G0D}:}{Object of class \code{data.table} created by
      \code{\link{fCompD}}. It includes daily
      values of:
      \describe{
        \item{Fd: }{numeric, the diffuse fraction}
        \item{Ktd: }{numeric, the clearness index}
        \item{G0d: }{numeric, the global irradiation on a horizontal surface
          (Wh/m²)}
        \item{D0d: }{numeric, the diffuse irradiation on a horizontal
          surface (Wh/m²)}
        \item{B0d: }{numeric, the direct irradiation on a horizontal surface
          (Wh/m²)}}
    }
    \item{\code{G0I}:}{Object of class \code{data.table} created by
      \code{\link{fCompI}}. It includes values of:
      \describe{
        \item{kt: }{numeric, clearness index}
        \item{G0: }{numeric, global irradiance on a horizontal surface, (W/m²)}
        \item{D0: }{numeric, diffuse irradiance on a horizontal surface, (W/m²)}
        \item{B0: }{numeric, direct irradiance on a horizontal surface, (W/m²)}
      }
      }
    \item{\code{G0dm}:}{Object of class \code{data.table} with monthly mean
      values of daily irradiation.}
    \item{\code{G0y}:}{Object of class \code{data.table} with yearly sums of irradiation.}
    \item{\code{Ta}:}{Object of class \code{data.table} with intradaily ambient
      temperature values.}
  }
  Besides, this class contains the slots from the \code{\linkS4class{Sol}} and
  \code{\linkS4class{Meteo}} classes.
}
\section{Extends}{
  Class \code{"\linkS4class{Meteo}"}, directly.
  Class \code{"\linkS4class{Sol}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as.data.tableD}{\code{signature(object = "G0")}: conversion
      to a data.table with daily values.}
    \item{as.data.tableI}{\code{signature(object = "G0")}: conversion
      to a data.table with intradaily values.}
    \item{as.data.tableM}{\code{signature(object = "G0")}: conversion
      to a data.table with monthly values.}
    \item{as.data.tableY}{\code{signature(object = "G0")}: conversion
      to a data.frame with yearly values.}
    \item{indexD}{\code{signature(object = "G0")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "G0")}: index of the
      \code{solI} slot. }
    \item{getLat}{\code{signature(object = "G0")}: latitude of the inherited
      \code{\linkS4class{Sol}} object. }
    \item{xyplot}{\code{signature(x = "G0", data = "missing")}: display
      the time series of daily values of irradiation.}
    \item{xyplot}{\code{signature(x = "formula", data = "G0")}:
      displays the contents of a \code{G0} object with the
      \code{xyplot} method for formulas.}

  }
}
\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \doi{10.18637/jss.v050.i09}
}}

\author{
  Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.
}


\seealso{
  \code{\linkS4class{Sol}},
  \code{\linkS4class{Gef}}.
}

\keyword{classes}
