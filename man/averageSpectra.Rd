\name{averageSpectra}
\alias{averageSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to average spectra.
}
\description{
This function accepts spectra, averages them, and returns the results. This function is a simple wrapper for the aggregate function, made to work smoothly with spectra.
}
\usage{
averageSpectra(spec = NULL, by = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
An object of class \code{spectra.list} or \code{spectra.matrix}.
}
  \item{by}{
Either (i) a vector of length \code{nrow(spec)} that specifies which combinations of spectra to average together, or (ii) a single numeric value specifying how many scans to combine, in order. Using the former option, the mean spectra will be named according to the values of by. For the latter option, they will be named in order numerically.
} 

}
\value{
Returns an object of class \code{spectra.matrix}. 
}
\author{
Daniel M Griffith}

\examples{

#data(shootout)
#by_vector <- gsub(pattern = "_.*",replacement = "", x = row.names(shootout_scans)) # average everything to one scan
#mean_scans <- averageSpectra(spec = shootout_scans, by = by_vector)
##or...
#mean_scans <- averageSpectra(spec = shootout_scans, by = 3) # average scans in groups of 3
#par(mfrow=c(1,2))
#plot(shootout_scans)
#plot(mean_scans)

}
\keyword{manipulation}
