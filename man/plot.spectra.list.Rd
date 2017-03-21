\name{plot.spectra.list}
\alias{plot.spectra.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot spectra.
}
\description{
This function accepts spectra in a \code{spectra.list} or \code{spectra.matrix} object and plots them.
}
\usage{
\method{plot}{spectra.list}(x, col = "black", type = NULL, xlim = NULL, ylim = NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{spectra.list} or \code{spectra.matrix} containing the spectra to plot.
}
  \item{col}{see par.
}
  \item{type}{see par.
}
  \item{xlim}{see par.
}
  \item{ylim}{see par.
}
  \item{...}{Additional args.
}
}

\author{
Daniel M Griffith}

\examples{

#data(shootout)
#plot(shootout_scans)

}
\keyword{visualization}
