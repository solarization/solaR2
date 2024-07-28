\name{C_corrFdKt}
\alias{corrFdKt}
\alias{FdKtPage}
\alias{FdKtLJ}
\alias{FdKtCPR}
\alias{FdKtEKDd}
\alias{FdKtCLIMEDd}
\alias{FdKtEKDh}
\alias{FdKtCLIMEDh}
\alias{FdKtBRL}
\Rdversion{1.0}
\encoding{UTF-8}

\title{Correlations between the fraction of diffuse irradiation and the
  clearness index.}
\description{A set of correlations between the fraction of diffuse irradiation and the
  clearness index used by \code{\link{fCompD}} and \code{\link{fCompI}}.
}
\usage{
## Monthly means of daily values
FdKtPage(Ktd)
FdKtLJ(Ktd)

## Daily values
FdKtCPR(Ktd)
FdKtEKDd(Ktd, sol)
FdKtCLIMEDd(Ktd)

## Intradaily values
FdKtEKDh(kt)
FdKtCLIMEDh(kt)
FdKtBRL(kt, sol)
}
\arguments{
  \item{Ktd}{A numeric, the daily clearness index.}

  \item{kt}{A numeric, the intradaily clearness index.}

  \item{sol}{A \code{Sol} object provided by \code{\link{calcSol}} or a
    \code{zoo} object provided by \code{\link{fSolD}} or \code{\link{fSolI}}.}
}

\value{A numeric, the diffuse fraction.}


\references{\itemize{
    \item Page, J. K., The calculation of monthly mean solar radiation for horizontal and inclined surfaces from
      sunshine records for latitudes 40N-40S. En U.N. Conference on New Sources of Energy, vol. 4, págs.
      378–390, 1961.
    \item Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
      diffuse and hemispherical and between daily and hourly insolation
      values. Solar Energy, 22:155–164, 1979.
    \item Erbs, D.G, Klein, S.A. and Duffie, J.A., Estimation of the diffuse
      radiation fraction for hourly, daily and monthly-average global
      radiation. Solar Energy, 28:293:302, 1982.
    \item De Miguel, A. et al., Diffuse solar irradiation model evaluation
      in the north mediterranean belt area, Solar Energy, 70:143-153, 2001.
    \item Ridley, B., Boland, J. and Lauret, P., Modelling of diffuse solar
      fraction with multiple predictors, Renewable Energy, 35:478-482, 2010.
  }
}
\seealso{
  \code{\link{fCompD}},
  \code{\link{fCompI}}
}
\examples{
lat = 37.2
BTd = fBTd(mode = 'prom')
G0dm = c(2.766, 3.491, 4.494, 5.912, 6.989, 7.742, 7.919, 7.027, 5.369,
    3.562, 2.814, 2.179)*1000;
Ta = c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
    15.2)
G0d = readG0dm(lat = lat, G0dm = G0dm, Ta = Ta)
sol = calcSol(lat = lat, BTd = BTd)
ktm = Ktm(sol = sol, G0d = G0d)
Monthly = data.table(Ktm = ktm)
Monthly$Page = FdKtPage(sol = sol, G0d = G0d)$Fd
Monthly$LJ = FdKtLJ(sol = sol, G0d = G0d)$Fd

xyplot(Page+LJ~Ktm, data = Monthly,
       type = c('l', 'g'), auto.key = list(space = 'right'))

ktd = Ktd(sol = sol, G0d = G0d)
Daily = data.table(Ktd = ktd)
Daily$CPR = FdKtCPR(sol = sol, G0d = G0d)$Fd
Daily$CLIMEDd = FdKtCLIMEDd(sol = sol, G0d = G0d)$Fd

xyplot(CPR + CLIMEDd ~ Ktd, data = Daily,
       type = c('l', 'g'), auto.key = list(space = 'right'))

}
\author{Oscar Perpiñán Lamigueiro; The BRL model was suggested by Kevin Ummel.}
\keyword{utilities}
