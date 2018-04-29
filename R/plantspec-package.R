#' @title plantspec: NIR Calibration and Spectral Data Management in R
#' 
#' @description This R package provides access to a global dataset of leaf NIR spectra and
#' contains tools for developing Partial Least Squares (PLS) regression models
#' for predicting leaf chemistry. Users can make use of pre-fitted
#' calibrations, add new data and fit new calibrations, and manage their
#' calibrations and datasets. The package represents an efficient workflow for
#' producing robust spectral calibrations.
#' 
#' @details Resources include:\cr - Functions for reading and writing spectra.
#' \cr - Tools for spectral manipulation and management (e.g., preprocessing,
#' conversion, subsetting).\cr - Tools for sample selection and experimental
#' design (e.g., Kennard-Stone selection).\cr - Wrapper functions for
#' optimization and fitting of spectral calibration models.\cr - Methods for
#' plotting spectra and models.\cr - Data and global calibrations for leaf C,
#' N, P, and K, documented in the companion data package \code{plantspecDB}.\cr
#'  - Data for examples, documented in \code{shootout}.
#' 
#' See the example below for a simple workflow for fitting a spectral
#' calibration model for nitrogen in a sample dataset.
#' 
#' @name plantspec-package
#' @aliases plantspec-package plantspec
#' @docType package
#' @author Daniel M Griffith <griffith.dan@@gmail.com>; T. Michael Anderson
#' <anderstm@@wfu.edu>
#' @references Bjorn-Helge Mevik, Ron Wehrens and Kristian Hovde Liland
#' (2013). pls: Partial Least Squares and Principal Component regression. R
#' package version 2.4-3. http://CRAN.R-project.org/package=pls
#' @keywords package
#' @import tools KernSmooth parallel StatMatch soil.spec reshape2 plotly pls hyperSpec
#' @examples
#' 
NULL

#' N_cal_shootout: Shootout NIR calibration for Nitrogen
#' 
#' NIR calibration for Nitrogen using the \code{shootout} data.
#' 
#' 
#' @name N_cal_shootout
#' @docType data
#' @format An object of class \code{PLScalibration}.
#' @references McClure, W., 1998. Software shootout at the idrc98. In: The
#' Ninth International Diffuse Reflectance Conference, Chambersburg,
#' Pennsylvania.
#' @keywords calibrations
#' @examples
#' 
NULL

#' Sample spectral data: software shootout at the IDRC98
#' 
#' Sample NIR and wet lab data for illustration. Dry plant material (Fescue
#' grass) from a fertilizer experiment were ground in a Wiley mill and scanned
#' on a FOSS NIRSystems spectrophotometer in 1998. Carbon and nitrogen data
#' were determined independently with a LECO CNS-2000.
#' 
#' For additional details about these data, visit:
#' 
#' \url{http://asrg.contactincontext.org/asrg/cnirs/Shoot%20Out%201998/shoot%20out%201998.html}
#' 
#' 
#' @name shootout
#' @aliases shootout_scans shootout_wetlab
#' @docType data
#' @format shootout_scans - a \code{spectra.matrix} containing the sample NIR
#' spectra.  shootout_wetlab - a \code{data.frame} with the nitrogen and carbon
#' data.
#' @references McClure, W., 1998. Software shootout at the idrc98. In: The
#' Ninth International Diffuse Reflectance Conference, Chambersburg,
#' Pennsylvania.
#' @keywords datasets
#' @examples
#' 
#' #data(shootout)
#' 
NULL



