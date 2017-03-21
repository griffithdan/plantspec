\name{as.spectra.matrix}
\alias{as.spectra.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert data to an object of class \code{spectra.matrix}.
}
\description{
Convert data to an object of class \code{spectra.matrix} from an object of class \code{spectra.list}.
}
\usage{
  \method{as}{spectra.matrix}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{spectra.list}.}
  \item{...}{
Additional args.}
}
\value{
Returns an object of class \code{spectra.matrix}.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#scans <- as.spectra.list(shootout_scans)
#scans <- as.spectra.matrix(scans)

}
\keyword{manipulation}
