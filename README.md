# <img src="https://github.com/stamats/MKomics/raw/master/hex-MKclass.png" alt="MKclass" width="120"/> &emsp; MKclass
The repository includes the development version of R package MKclass

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MKclass)](http://cran.r-project.org/package=MKclass)
[![cran checks](https://cranchecks.info/badges/summary/MKclass)](https://cran.r-project.org/web/checks/check_results_MKclass.html)


## Description
Performance measures and scores for statistical classification such as accuracy, 
sensitivity, specificity, recall, similarity coefficients, AUC, GINI index, 
Brier score and many more. Calculation of optimal cut-offs and decision stumps 
(Iba and Langley (1991), <doi:10.1016/B978-1-55860-247-2.50035-8>) for all 
implemented performance measures. Hosmer-Lemeshow goodness of fit tests 
(Lemeshow and Hosmer (1982), <doi:10.1093/oxfordjournals.aje.a113284>; 
Hosmer et al (1997), <doi:10.1002/(SICI)1097-0258(19970515)16:9%3C965::AID-SIM509%3E3.0.CO;2-O>). 
Statistical and epidemiological risk measures such as relative risk, odds ratio, 
number needed to treat (Porta (2014), <doi:10.1093%2Facref%2F9780199976720.001.0001>).


## Installation

```{r, eval = FALSE}
## Installation of CRAN version
install.packages("MKclass")

## Install package remotes
if(!require(remotes)) install.packages("remotes")
## Install package MKclass
remotes::install_github("stamats/MKclass", build_vignettes = TRUE)
```

## Getting started

```{r}
library(MKclass)
```
