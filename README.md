
<!-- README.md is generated from README.Rmd. Please edit that file -->

# panelView

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stablel)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![downloads:
CRAN](https://cranlogs.r-pkg.org/badges/grand-total/panelView)](https://www.datasciencemeta.com/rpackages)
<!-- badges: end -->

**Authors:** Hongyu Mou (UCLA); Licheng Liu (MIT); [Yiqing
Xu](https://yiqingxu.org/) (Stanford)

**Date:** Feb 12, 2023

**Repos:** [Github](https://github.com/xuyiqing/panelView) (1.1.16)
[CRAN](https://cran.r-project.org/web/packages/panelView/index.html)
(1.1.11)

**Examples:** R code used in the tutorial can be downloaded from
[here](https://github.com/xuyiqing/panelView/blob/master/examples.R).

------------------------------------------------------------------------

## Description

**panelView** visualizes panel data. It has three main functionalities:

1.  it plots treatment status and missing values in a panel dataset;
2.  it plots the temporal dynamics of an outcome variable (or any
    variable) in a panel dataset;
3.  it visualizes bivariate relationships of two variables by unit or in
    aggregate.

## Installation

You can install the up-to-date development version from GitHub:

``` r
# if not already installed
install.packages('devtools', repos = 'http://cran.us.r-project.org') 

# note: "V" is capitalized
devtools::install_github('xuyiqing/panelView') 
```

You can also install the **panelView** package from CRAN:

``` r
install.packages('panelView') 
```

If you encounter an installation/execution error, please remove the old
package and reinstall **panelView**.

``` r
remove.packages('panelView') 
# or
remove.packages('panelview') # package name "panelview" no longer in use
```

## Tutorial & Paper

For example, plot treatment status in a panel dataset:

``` r
library(panelView)
data(panelView)
panelview(turnout ~ policy_edr + policy_mail_in + policy_motor, 
          data = turnout, index = c("abb","year"), 
          xlab = "Year", ylab = "State")
```

Note that “V” in the package name is capitalized while “v” in the
function name is not—to be consistent with the Stata version.

See the
[tutorial](https://yiqingxu.org/packages/panelview/articles/tutorial.html)
page for more details.

For a paper version of the tutorial, see [Mou, Liu & Xu
(2022)](http://ssrn.com/abstract=4202154): “panelView: Panel Data
Visualization in R and Stata.”

## Report bugs

Please report bugs to **yiqingxu \[at\] stanford.edu** with your sample
code and data file. Much appreciated!
