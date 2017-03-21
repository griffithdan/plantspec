\name{combineSpectra}
\alias{combineSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to combine spectral datasets into one object.
}
\description{
This function accepts two objects of class \code{spectra.list} or \code{spectra.matrix} and combines them into one \code{spectra.matrix}. These objects should be in the same units and of the same spectral resolution and extend.
}
\usage{
combineSpectra(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
  \item{y}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
}
\value{
Returns a \code{spectra.matrix}.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#shootout_scans_x_2 <- combineSpectra(x = shootout_scans, y = shootout_scans)


}
\keyword{manipulation}
