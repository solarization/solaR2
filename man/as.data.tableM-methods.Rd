\name{D_as.data.tableM-methods}
\docType{methods}
\alias{as.data.tableM}
\alias{as.data.tableM-methods}
\alias{as.data.tableM,G0-method}
\alias{as.data.tableM,Gef-method}
\alias{as.data.tableM,ProdGCPV-method}
\alias{as.data.tableM,ProdPVPS-method}

\title{Methods for Function as.data.tableM}
\description{Convert a \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{as.data.table} object with monthly average of daily values.}

\usage{
\S4method{as.data.tableM}{G0}(object, complete=FALSE, day=FALSE)
}

\arguments{

  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  \item{day}{A logical}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{The result is the \code{G0dm}
  slot. If \code{day=TRUE} (default is \code{FALSE}), the result
  includes two columns names \code{month} and \code{year}.
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE}
  (default) the result is the slot \code{Gefdm}. If
  \code{complete=TRUE} it returns the slot \code{G0dm}.
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result is the \code{prodDm} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0dm} and \code{Gefdm}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result is the \code{prodDm} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0dm} and \code{Gefdm}.
}

}}

\examples{
lat = 37.2
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562,
          2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2,
        17.2, 15.2)
prom <- list(G0dm = G0dm, Ta = Ta)
prodfixed = prodGCPV(lat, dataRad = prom)
prodM = as.data.tableM(prodfixed, complete = TRUE, day = TRUE)
prodM
}

\keyword{methods}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}
