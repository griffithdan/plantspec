\name{subdivideDataset}
\alias{subdivideDataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects a subset of a multivariate (e.g., spectral) dataset.
}
\description{
This function accepts spectra in a \code{spectra.list} or \code{spectra.matrix} object and selects a subset of that dataset. Importantly, the function can be set to select either a calibration or validation subset. These are fundamentally different. When you select a calibration dataset the intention is to choose a representative subset of all spectral data on which to perform wet lab analysis. However, when selecting a subset of samples (which already have wet lab analysis) in order to validate a model, it is important that both the validation (test set) and and calibration (training set) are representative--otherwise, the calibration model will be fit to well sampled spectral space but validated on outlying points. The calibration selection uses the Kennard-Stone algorithm whereas the validation selection uses the Duplex algorithm, which is a modification the original author's proposed. Finally, this function can also perform calibration or validation selection in one of five distinct methods (see the method parameter for details).
}
\usage{
subdivideDataset(spectra, component = NULL, type = "validation", p = 0.2, method = "KS", seed.set = NULL, output = "logical")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spectra}{
An object of class \code{spectra.list} or \code{spectra.matrix} containing the spectra to write.
}
  \item{component}{
Method "SPXY" and "MDKS" incorporate Y-value data in subset selection. If using one of these two methods, a vector of Y data should be provided here.
}
  \item{type}{
One of "calibration" or "validation" depending on the type of subset required.
}
  \item{p}{
The proportion of the dataset to select as the "calibration" or "validation" group.
}
  \item{method}{
The desired method. Selected from:\cr
  "KS" - Standard, Kennard-Stone selection. When \code{type = validation} the performs Duplex selection.\cr
  "PCAKS" - Selection is performed on the principal components from a PCA of the spectra.\cr
  "SPXY" - Selection occurs on both X (spectra) and Y (component) data with equal weighting.\cr
  "MDKS" - Mahalanobis distance is used instead of euclidean distances. Selection occurs on both X (spectra) and Y (component) data with equal weighting.\cr
  "random" - Simple random selection, regardless of multivariate distribution.
}
  \item{seed.set}{
A single numeric value. If method is "random" then you can set the seed so that the same selection is produced each time.
}
  \item{output}{
One of "logical" or "names." If "logical" then the function will return a logical vector where TRUE values are the selected samples. If "names" then the names of the selected spectra are returned.
}
}
\value{
A vector. Depending on \code{output}, either a logical of list of names indicating selected spectra.
}

\references{
Kennard, R. W. and Stone, L. A. (1969) Computer aided design of experiments. Technometrics, 11, 137-148.

Galvao, R., Araujo, M., Jose, G., Pontes, M., Silva, E. & Saldanha, T. (2005). A method for calibration and validation subset partitioning. Talanta, 67, 736–740.

Saptoro, Agus; Tadé, Moses O.; and Vuthaluru, Hari (2012) "A Modified Kennard-Stone
Algorithm for Optimal Division of Data for Developing Artificial Neural Network Models,"
Chemical Product and Process Modeling: Vol. 7: Iss. 1, Article 13. DOI: 10.1515/1934-2659.1645 

Snee, R.D., 1977. Validation of regression models: methods and examples. Technometrics, 19, 415-428.
}

\author{
Daniel M Griffith}

\examples{

#data(shootout)
#val_set <- subdivideDataset(spectra = shootout_scans, type = "validation", method = "KS")
#table(val_set)

}
\keyword{manipulation}
