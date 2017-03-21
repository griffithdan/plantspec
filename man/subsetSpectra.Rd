\name{subsetSpectra}
\alias{subsetSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to subset regions of spectra.
}
\description{
This function accepts an object containing spectra and subsets it to a specified region(s).
}
\usage{
subsetSpectra(spec, ranges)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
An object of class \code{spectra.matrix} to be subset.
}
  \item{ranges}{
A two column matrix, each row contains max and min range values. 
}
}
\value{
Returns an object of class \code{spectra.list}.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#s_mat <- matrix(data = c(2000,1750,1000,500), nrow = 2, ncol = 2, byrow = T)
#sub_data <- subsetSpectra(spec = shootout_scans, ranges = s_mat)

#par(mfrow=c(1,2))
#plot(shootout_scans)
#plot(sub_data)

}
\keyword{manipulation}
