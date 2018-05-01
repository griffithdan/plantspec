Data on plant elemental stoichiometry are essential for ecologists investigating ecosystem function, nutrient cycling, plant-herbivore interactions, and leaf economics. A cost effective approach to leaf elemental analysis is near-infrared spectroscopy (NIRS), whereby components such as carbon (C), nitrogen (N), phosphorus (P), and potassium (K) can be predicted using NIR spectra from plant material. However, a major factor limiting the widespread use of NIRS by plant ecologists is the availability of free software and calibration data for developing plant chemistry datasets from spectra. Here, we present a pair of companion R packages (‘plantspec’ and ‘plantspecDB’) that satisfy this need by providing an entire workflow, from spectra to predicted elemental data. The main R package, called ‘plantspec’, allows users to manipulate spectral data and develop custom partial least square (PLS) models for predicting the elemental composition of their own datasets. The second package, ‘plantspecDB’, provides NIR spectra, and matched elemental data obtained with standard analytical techniques, for herbaceous samples collected from 18 grassland sites around the world, primarily sourced from the Nutrient Network experiment. The ‘plantspecDB’ data package also provides calibrations for C, N, P, and K for bulk samples and separated by plant functional type (grasses, forbs and legumes). 

Here, we provide an example of the ‘plantspec’ workflow that produces an NIR calibration for N, similar to the one reported in Anderson et al. (2018). This workflow provides a template to produce robust calibration models and increase the application of NIR in ecology. Although we focus on plant stoichiometry, the workflow presented here can be applied broadly for a broad range of applications, including soils, remote sensing data, solutions and tissues. Finally, our global dataset provides unprecedented access to NIR calibration data as they pertain to tissue concentrations of key plant limiting elements. We hope that 'plantspec' will help to form a community of users who share data in order to increase the utility of NIR for ecologists, and we provide a submission portal to facilitate data sharing: [plantspec submission portal](https://griffithdan.github.io/pages/code_and_data/plantspec/data-submission.html).

[Check out our vignette for a full tutorial for using 'plantspec!'](https://griffithdan.github.io/pages/code_and_data/plantspec.html)

## Functionality in 'plantspec'

- Functions for reading and writing spectra (currently spc, dpt, txt, and OPUS files). 
- Tools for spectral manipulation and management (e.g., preprocessing, conversion, subsetting).
- Tools for sample selection and experimental design (e.g., Modified Kennard-Stone selection).
- Wrapper functions for optimization and fitting of spectral calibration models.
- Methods for (interactive) plotting spectra and models.
- Tools for evaluating model output (e.g., Mahalanobis distance).
- Data and global calibrations for leaf C, N, P, and K, documented in the companion data package plantspecDB.
- Data for examples, documented in shootout data from McClure (1998).

## Installation

Install 'plantspec' and 'plantspecDB' from GitHub using devtools.

```r
library(devtools)
install_github(repo = "griffithdan/plantspec") # Install analysis package
  library(plantspec)
install_github(repo = "griffithdan/plantspecDB") # Install data package 
  library(plantspecDB)
```
