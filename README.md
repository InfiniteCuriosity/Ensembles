
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ensembles

<!-- badges: start -->
<!-- badges: end -->

The goal of Ensembles is to use the power of both R and Python packages
to automatically solve four types of supervised learning problems:
Regression, Classification, Logistic Analysis, and Forecasting (time
series). Each of the four functions completes the entire data science
process automatically, and presents the finished results to the user.
This includes splitting the data set up (train, test and validation),
randomly automatically resampling the data sets, automatically building
individual models using R and Python, automatically building weighted
ensemble models from the predictions of the R and Python models,
automatically sorting the output and presenting it to the user. The
output is both visual (graphs and charts) and analytic (tables). The
user is able to see all the results, and select a different top result
if that is appropriate.

## Installation

You can install the development version of Ensembles like so:

``` r
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Ensembles)
#> Loading required package: arm
#> Loading required package: MASS
#> Loading required package: Matrix
#> Loading required package: lme4
#> 
#> arm (Version 1.13-1, built: 2022-8-25)
#> Working directory is /Users/russconte/Library/Mobile Documents/com~apple~CloudDocs/Documents/Machine Learning templates in R/miniensemble copy
#> Loading required package: brnn
#> Loading required package: Formula
#> Loading required package: truncnorm
#> Loading required package: broom
#> Loading required package: C50
#> Loading required package: class
#> Loading required package: corrplot
#> corrplot 0.92 loaded
#> 
#> Attaching package: 'corrplot'
#> The following object is masked from 'package:arm':
#> 
#>     corrplot
#> Loading required package: Cubist
#> Loading required package: lattice
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:MASS':
#> 
#>     select
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: e1071
#> Loading required package: fable
#> Loading required package: fabletools
#> Registered S3 method overwritten by 'tsibble':
#>   method          from
#>   format.interval inum
#> 
#> Attaching package: 'fabletools'
#> The following object is masked from 'package:e1071':
#> 
#>     interpolate
#> The following object is masked from 'package:lme4':
#> 
#>     refit
#> Loading required package: fable.prophet
#> Loading required package: Rcpp
#> Loading required package: feasts
#> Loading required package: gam
#> Loading required package: splines
#> Loading required package: foreach
#> Loaded gam 1.22-3
#> Loading required package: gbm
#> Loaded gbm 2.1.9
#> This version of gbm is no longer under development. Consider transitioning to gbm3, https://github.com/gbm-developers/gbm3
#> Loading required package: ggplot2
#> Loading required package: glmnet
#> Loaded glmnet 4.1-8
#> Loading required package: gridExtra
#> 
#> Attaching package: 'gridExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
#> Loading required package: gt
#> Loading required package: gtExtras
#> 
#> Attaching package: 'gtExtras'
#> The following object is masked from 'package:MASS':
#> 
#>     select
#> Loading required package: ipred
#> Loading required package: kernlab
#> 
#> Attaching package: 'kernlab'
#> The following object is masked from 'package:ggplot2':
#> 
#>     alpha
#> Loading required package: klaR
#> Loading required package: leaps
#> Loading required package: MachineShop
#> 
#> Attaching package: 'MachineShop'
#> The following objects are masked from 'package:fabletools':
#> 
#>     accuracy, response
#> The following object is masked from 'package:stats':
#> 
#>     ppr
#> Loading required package: magrittr
#> Loading required package: mda
#> Loaded mda 0.5-4
#> 
#> Attaching package: 'mda'
#> The following object is masked from 'package:MachineShop':
#> 
#>     confusion
#> Loading required package: Metrics
#> 
#> Attaching package: 'Metrics'
#> The following objects are masked from 'package:MachineShop':
#> 
#>     accuracy, auc, mae, mse, msle, precision, recall, rmse, rmsle
#> The following object is masked from 'package:fabletools':
#> 
#>     accuracy
#> Loading required package: neuralnet
#> 
#> Attaching package: 'neuralnet'
#> The following object is masked from 'package:dplyr':
#> 
#>     compute
#> Loading required package: pls
#> 
#> Attaching package: 'pls'
#> The following object is masked from 'package:corrplot':
#> 
#>     corrplot
#> The following objects are masked from 'package:arm':
#> 
#>     coefplot, corrplot
#> The following object is masked from 'package:stats':
#> 
#>     loadings
#> Loading required package: pROC
#> Type 'citation("pROC")' for a citation.
#> 
#> Attaching package: 'pROC'
#> The following object is masked from 'package:Metrics':
#> 
#>     auc
#> The following object is masked from 'package:MachineShop':
#> 
#>     auc
#> The following objects are masked from 'package:stats':
#> 
#>     cov, smooth, var
#> Loading required package: purrr
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
#> The following object is masked from 'package:MachineShop':
#> 
#>     lift
#> The following object is masked from 'package:kernlab':
#> 
#>     cross
#> The following objects are masked from 'package:foreach':
#> 
#>     accumulate, when
#> Loading required package: randomForest
#> randomForest 4.7-1.1
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: 'randomForest'
#> The following object is masked from 'package:gridExtra':
#> 
#>     combine
#> The following object is masked from 'package:ggplot2':
#> 
#>     margin
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
#> Loading required package: reactable
#> Loading required package: reactablefmtr
#> 
#> Attaching package: 'reactablefmtr'
#> The following object is masked from 'package:randomForest':
#> 
#>     margin
#> The following objects are masked from 'package:gt':
#> 
#>     google_font, html
#> The following object is masked from 'package:ggplot2':
#> 
#>     margin
#> Loading required package: readr
#> Loading required package: rpart
#> Loading required package: scales
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> The following object is masked from 'package:kernlab':
#> 
#>     alpha
#> The following object is masked from 'package:arm':
#> 
#>     rescale
#> Loading required package: tibble
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:magrittr':
#> 
#>     extract
#> The following objects are masked from 'package:Matrix':
#> 
#>     expand, pack, unpack
#> Loading required package: tree
#> Loading required package: tsibble
#> 
#> Attaching package: 'tsibble'
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union
#> Loading required package: xgboost
#> 
#> Attaching package: 'xgboost'
#> The following object is masked from 'package:dplyr':
#> 
#>     slice
#> 
#> Attaching package: 'Ensembles'
#> The following object is masked from 'package:MASS':
#> 
#>     Boston
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
