
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ensembles: The data science set of tools I have always wanted but could not find anywhere, so I made it myself.

<!-- badges: start -->
<!-- badges: end -->

The goal of Ensembles is to use the power of both R and Python packages
to automatically solve four types of supervised learning problems:
Regression, Classification, Logistic Analysis, and Forecasting (time
series). Each of the four functions in the Ensembles package completes
the entire data science process automatically, and presents the finished
results to the user. This includes splitting the data set up (train,
test and validation), randomly resampling the data sets, automatically
building individual models using R and Python, automatically building
weighted ensemble models from the predictions of the R and Python
models, automatically sorting the output and presenting it to the user.
The output is both visual (graphs and charts) and analytic (tables). The
user is able to see all the results, and select a different top result
if that is appropriate.

All of the models are built automatically, and do not return any errors
or warnings, there are no typos, no grammar errors, no misplaced commas
or periods, no need to correct anything in any of the models.

The user only needs to enter one line of code. The one line of code
tells the package about the data (where is it located, which feature is
the target feature), and how to handle advanced functions (such as
automatically removing highly correlated predictors at a value the user
chooses, among other options).

There are sample data sets included with the package to demonstrate all
of the features that are available.

## Installation

You can install the development version of Ensembles like so:

``` r
devtools::install_github("InfiniteCuriosity/Ensembles")
#> Downloading GitHub repo InfiniteCuriosity/Ensembles@HEAD
#> QuickJSR  (1.0.9  -> 1.1.0 ) [CRAN]
#> xts       (0.13.1 -> 0.13.2) [CRAN]
#> paletteer (1.5.0  -> 1.6.0 ) [CRAN]
#> Installing 3 packages: QuickJSR, xts, paletteer
#> Installing packages into '/private/var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T/RtmpkSvpCg/temp_libpathe0c74e1456e'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T//Rtmpq0h4Bc/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T/Rtmpq0h4Bc/remotesfc8204c1777/InfiniteCuriosity-Ensembles-35e6393/DESCRIPTION’ ... OK
#> * preparing ‘Ensembles’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘Ensembles_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T/RtmpkSvpCg/temp_libpathe0c74e1456e'
#> (as 'lib' is unspecified)
```

### Examples: More details are available in the vignettes, please read those once you are finished with the ReadMe. You can see all vignettes on your system with the command: browseVignettes()

## Example: Numerical analysis

This is a basic example which shows you how to solve a common problem:
Modeling the median value of a house in the Boston housing data set. The
results here beat the 699 results submitted by college students, here:
<https://www.kaggle.com/competitions/uou-g03784-2022-spring/leaderboard>

The text for the numerical analysis is as follows (it is presented as
text here, as this will take a couple of minutes to run if presented in
actual code, try it yourself to see how it runs):

numerical(data = MASS::Boston, colnum = 14, numresamples = 5,
how_to_handle_strings = 0, do_you_have_new_data = “N”,
save_all_trained_models = “N”, remove_ensemble_correlations_greater_than
= 2.00, train_amount = 0.60, test_amount = 0.20, validation_amount =
0.20)

## Example: Logistic analysis

This example creates predictive models from the Diabetes data set
included in the Ensembles package. Like the other functions in the
Ensembles package (numerical, classification and forecasting), logistic
uses R and Python, individual and ensemble models, regular and deep
learning.

The text for the logistic analysis is as follows (it is presented as
text here, as this will take a couple of minutes to run if presented in
actual code, try it yourself to see how it runs):

library(Ensembles)

logistic(data = diabetes, colnum = ncol(diabetes), numresamples = 5,
save_all_trained_models = “N”, how_to_handle_strings = 0,
do_you_have_new_data = “N”, remove_ensemble_correlations_greater_than =
2.00, train_amount = 0.60, test_amount = 0.20, validation_amount = 0.20)

## Example: Classification analysis

This example models the location of Carseats from the Carseats dataset
in the ISLR package. There are three options for the location: Good,
Bad, and Medium. The classification analysis automatically builds all
the models, presents all the solution matrices, summary tables, and much
more.

Similar to logistic analysis, the user does not need to set anything up,
other than making sure the target column is a factor. It does not matter
how many levels the factor has, the function will automatically make the
accurate solution matrices, tables, and all other results.

The text for the logistic analysis is as follows (it is presented as
text here, as this will take a couple of minutes to run if presented in
actual code, try it yourself to see how it runs):

library(Ensembles)

classification(data = ISLR::Carseats, colnum = 7, numresamples = 5,
do_you_have_new_data = “N”, how_to_handle_strings = 1,
save_all_trained_models = “N”, train_amount = 0.60, test_amount = 0.20,
validation_amount = 0.20)

## Example: Forecasting analysis
