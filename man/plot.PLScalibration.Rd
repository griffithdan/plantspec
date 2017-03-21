\name{plot.PLScalibration}
\alias{plot.PLScalibration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize PLS modeling results.
}
\description{
This function is a wrapper to the plot methods in the pls R package. It takes an object of class \code{PLScalibration} and produces diagnostic plots.
}
\usage{
\method{plot}{PLScalibration}(x, pch = 21, bg = "gray", ncomp = NULL, plottype = "prediction", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{PLScalibration} containing the model.
}
  \item{pch}{see par.
}
  \item{bg}{see par.
}
  \item{ncomp}{The number of latent vectors to use. If NULL, the default is to use the optimal number of determined during the optimization process.
}
  \item{plottype}{
One of:\cr
  "prediction" - Predicted versus observed.\cr
  "validation" - RMSEP versus rank.\cr
  "coefficients" - Coefficients.\cr
  "scores" - Biplot of scores.\cr
  "loadings" - Loadings.
}
  \item{...}{Additional args.
}
}
\value{
A plot of model results.
}
\author{
Daniel M Griffith}

\examples{

#data(N_cal_shootout)
#plot(N_cal_shootout)

}
\keyword{visualization}
