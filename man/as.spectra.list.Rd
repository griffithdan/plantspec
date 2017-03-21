\name{as.spectra.list}
\alias{as.spectra.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert data to an object of class \code{spectra.list}.
}
\description{
Convert data to an object of class \code{spectra.list} from an object of class \code{spectra.matrix}.
}
\usage{
  \method{as}{spectra.list}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{spectra.matrix}.}
  \item{...}{
Additional args.}
}
\value{
Returns an object of class \code{spectra.list}.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#scans <- as.spectra.list(shootout_scans)

}
\keyword{manipulation}
