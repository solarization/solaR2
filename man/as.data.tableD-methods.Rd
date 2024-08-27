\name{D_as.data.tableD-methods}
\docType{methods}
\alias{as.data.tableD}
\alias{as.data.tableD-methods}
\alias{as.data.tableD,G0-method}
\alias{as.data.tableD,Gef-method}
\alias{as.data.tableD,ProdGCPV-method}
\alias{as.data.tableD,ProdPVPS-method}
\alias{as.data.tableD,Sol-method}

\title{Methods for Function as.data.tableD}
\description{Convert a \code{Sol}, \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{data.table} object with daily values.}

\usage{
\S4method{as.data.tableD}{Sol}(object, complete=FALSE, day=FALSE)
}

\arguments{

  \item{object}{A \code{Sol} object (or extended.)}
  \item{complete}{A logical.}
  \item{day}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "Sol")}}{Conversion to a \code{data.table}
  object with the content of the \code{solD} slot. If \code{day=TRUE}
  (default is \code{FALSE}), the result includes three columns named
  \code{month}, \code{day} (day of the year) and \code{year}. 
}

\item{\code{signature(object = "G0")}}{If \code{complete=FALSE} (default) the result includes only
  the columns of \code{G0d}, \code{D0d} and \code{B0d} from the
  \code{G0D} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD}
  and \code{G0D}.
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE} (default) the result includes only
  the columns of \code{Gefd}, \code{Defd} and \code{Befd} from the
  \code{GefD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D} and \code{GefD}
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result includes only the columns of \code{Eac},
  \code{Edc} and \code{Yf} from the \code{prodD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D}, \code{GefD} and \code{prodD}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result includes only the columns of \code{Eac},
  \code{Qd} and \code{Yf} from the \code{prodD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D}, \code{GefD} and \code{prodD}.
}
}}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}
