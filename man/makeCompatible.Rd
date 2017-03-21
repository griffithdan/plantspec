\name{makeCompatible}
\alias{makeCompatible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Manipulate a spectral dataset to conform to the units, resolution, and extent of a reference dataset.
}
\description{
This function accepts spectra as a \code{spectra.list} or \code{spectra.matrix} object manipulates it for conform to to the units,resolution, and extent of a reference dataset.
}
\usage{
makeCompatible(x, ref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{spectra.list} or \code{spectra.matrix} containing the spectra to modify.
}
  \item{ref}{
An object of class \code{spectra.list} or \code{spectra.matrix} have the attributes (units, resolution, etc.) desired for \code{x}.
}

}
\value{
Returns an object of class \code{spectra.list} or \code{spectra.matrix} containing the modified spectra.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#reference <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
#reference <- reference[,1:100] 
#makeCompatible(x = shootout_scans, ref = reference)

}
\keyword{manipulation}
