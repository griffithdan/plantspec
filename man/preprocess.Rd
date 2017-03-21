\name{preprocess}
\alias{preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to preprocess on spectra.
}
\description{
This function accepts spectra, performs the desired transformations, and returns the results.
}
\usage{
preprocess(spec, transformation, MSC_reference = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
  \item{transformation}{
The desired transformation. Selected from:\cr
  "RAW" - The raw spectrum, as supplied, is returned.\cr
  "D1f" - The first derivative.\cr
  "D2f" - The second derivative.\cr
  "COE" - Constant Offset Elimination.\cr
  "SLS" - Straight Line Subtraction.\cr
  "SNV" - Vector Normalization.\cr
  "MMN" - Min/Max Normalization.\cr
  "MSC" - Multiplicative Scattering Correction.
} 
  \item{MSC_reference}{
A reference used in MSC prepreocessing. If NULL, the mean spectra of the provided spectra is used. 
}
}
\value{
Returns an object of class \code{spectra.matrix}. If MSC was performed, the mean/reference spectra is returned as an attribute called "MSC_reference."
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#processed_data <- preprocess(spec = shootout_scans, transformation = "COE")
#par(mfrow=c(1,2))
#plot(shootout_scans)
#plot(processed_data)

}
\keyword{manipulation}
