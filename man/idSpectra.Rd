\name{idSpectra}
\alias{idSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify spectra in a plot.
}
\description{
This function accepts spectra (the same ones given to \code{plot()}), allows the user to click a scan, then returns the scan name in the console and colors the selected scan red.
}
\usage{
idSpectra(spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
}
\value{
Returns an the name of the selected scan.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#plot(shootout_scans)
#idSpectra(shootout_scans)

}
\keyword{visualization}
