\name{readSpectra}
\alias{readSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to read spectra.
}
\description{
This function accepts a vector of file paths specifying files that contain spectra. The function incorporates these spectra into a single \code{spectra.matrix} object. The function supports .dpt, .txt, opus binary (e.g., .0), and .spc files. Files import for .spc files is done using the package \code{hpyerSpec} and import of opus format is done using the package \code{soil.spec}. These formats are auto detected. Feel free to request/suggest file formats.
}
\usage{
readSpectra(filelist, wave_unit = "wavenumber", measurement_unit = "absorbance")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filelist}{
A vector (e.g., the output of \code{list.files(full.names = TRUE)}) containing the file paths for spectra to read.
}
  \item{wave_unit}{
"wavenumber" or "wavelength".
}
  \item{measurement_unit}{
"absorbance" or "transmittance".
}
}
\value{
Returns an object of class \code{spectra.matrix}.
}
\author{
Daniel M Griffith}

\examples{

## MY_PATH = is a text string identifying the folder with the spectra 
# specs <- readSpectra(filelist = list.files("MY_PATH", full.names = T))

}
\keyword{read/write/fileio}
