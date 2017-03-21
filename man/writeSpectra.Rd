\name{writeSpectra}
\alias{writeSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to write spectra.
}
\description{
This function accepts spectra in a single \code{spectra.list} or \code{spectra.matrix} object and writes those spectra to separate files. Currently this function only writes ".dpt" files.
}
\usage{
writeSpectra(spectra, filelist=NULL, path=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spectra}{
An object of class \code{spectra.list} containing the spectra to write.
}
  \item{filelist}{
A vector of full file names, in the same order as the elements of \code{spectra}.
}
  \item{path}{
A string specifying a directory, where all spectra will be written using \code{names(spectra)} as file names. Not used with \code{filelist} - ignored if \code{filelist} is specified.
}
}
\value{
Writes spectra to files.
}
\author{
Daniel M Griffith}

\examples{

# data(shootout)
# writeSpectra(spectra = shootout_scans, path = "MY_PATH")

}
\keyword{read/write/fileio}
