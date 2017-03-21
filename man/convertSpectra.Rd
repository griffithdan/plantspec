\name{convertSpectra}
\alias{convertSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts spectra into new units.
}
\description{
This function accepts spectra in a single \code{spectra.list} or \code{spectra.matrix} object performs a specified units conversion.
}
\usage{
convertSpectra(x, method = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
  \item{method}{
The desired conversion. Selected from:\cr
  "A_to_TR" - Absorbance to Transmittance.\cr
  "TR_to_A" - Transmittance to Absorbance.\cr
  "WN_to_WL" - Wavenumbers to Wavelength.\cr
  "WL_to_WN" - Wavelength to Wavenumbers.
}

}
\value{
A \code{spectra.matrix} containing the converted spectra.
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#attributes(shootout_scans)
#converted_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")


}
\keyword{manipulation}
