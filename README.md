
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ensembles: The data science set of tools I always wanted but could not find anywhere, so I made it myself.

<!-- badges: start -->

[![R-CMD-check](https://github.com/InfiniteCuriosity/Ensembles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InfiniteCuriosity/Ensembles/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

My favorite solutions always involve addressing real problems from real
customers. I’ve managed multi-million dollar accounts for Fortune 1000
companies, hired more than 10,000 people, and worked in (and ran)
several non-profit organizations (some volunteer, some paid). The value
that data science can bring to organizations is crystal clear to me
based on my extensive experience. However, in looking for data science
solutions to real customer issues, I could not find what I was looking
for, so I made it myself.

The goal of Ensembles is to use the power of both R and Python packages
to automatically solve four types of supervised learning problems:
Regression, Classification, Logistic Analysis, and Forecasting (time
series). Each of the four functions in the Ensembles package completes
the entire data science process automatically, and presents the finished
results to the user. This includes splitting the data set up (train,
test and validation), randomly resampling the data sets as many times as
the user indicates, automatically building 23 individual models using R
and Python, automatically building 17 additional weighted ensemble
models from the predictions of the R and Python models, automatically
sorting the output and presenting it to the user. The output is both
visual (graphs and charts) and analytic (tables). The user is able to
see all the results, and select a different top result if that is
appropriate.

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
#> rstudioapi (0.15.0 -> 0.16.0) [CRAN]
#> Installing 1 packages: rstudioapi
#> 
#> The downloaded binary packages are in
#>  /var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T//RtmplmoCwW/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/private/var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T/RtmplmoCwW/remotes71bd3be28baa/InfiniteCuriosity-Ensembles-e937b54/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/xv/cqn_nl890vx7jgr4tpq5dsj40000gq/T/RtmplmoCwW/remotes71bd3be28baa/InfiniteCuriosity-Ensembles-e937b54/DESCRIPTION’
#>   ─  preparing ‘Ensembles’:
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘Ensembles_0.1.0.tar.gz’
#>      
#> 
```

## Basic Example: Numerical analysis

This is a basic example which shows you how to solve a common problem:
Modeling the median value of a house in the Boston housing data set. The
results here beat the 699 results submitted by college students, here:
<https://www.kaggle.com/competitions/uou-g03784-2022-spring/leaderboard>

The text for the numerical analysis is as follows (it is presented as
text here, as this will take a couple of minutes to run if presented in
actual code, try it yourself to see how it runs):

library(Ensembles)

numerical(data = MASS::Boston, colnum = 14, numresamples = 5,
how_to_handle_strings = 0, do_you_have_new_data = “N”,
save_all_trained_models = “N”, remove_ensemble_correlations_greater_than
= 1.00, use_parallel = “N”, train_amount = 0.60, test_amount = 0.20,
validation_amount = 0.20)

Once the code has completed running, **look in the Console for**:

Data dictionary (lists each of the features, the type of feature it is
(for example, numerical or integer), and the first values in the data

The correlation report of the original data (in this case the Boston
Housing data set)

A data summary that shows the minimum, maximum, and several other values
for each feature

The head of the ensemble.

The Ensemble correlation table

**Look in the Plots window for:**

Pairwise scatter plots and histograms of the numerical data

Correlation plot of the numerical data (as numbers)

Correlation plot of the numerical data (as colored circles)

Three plots: Best model predicted vs actual, best model residuals and
best model histogram of residuals

Best model predicted vs actual

Best model Residuals

Best model histogram of residuals

Accuracy data on all 40 models in alphabetical order. The black line is
the mean, each dot is a specific result for that model.

Accuracy data on all 40 models used in alphabetical order, also showing
the results for train, test and validation data

Histograms of each numeric column

Box plots of the numeric data

**Look in the Viewer tab for:**

Summary report sorted by accuracy (lowest RMSE at top), including:

Model name

Mean holdout RMSE (holdout = mean of test and validation data)

Standard deviation of holdout RMSE

Mean of the data

Standard deviation of the data

Mean of train RMSE

Mean of test RMSE

Mean of validation RMSE

Overfitting (mean of holdout RMSE / mean of training RMSE)

Duration

Note the table is searchable, and sortable

## Basic Example: Logistic analysis

This example creates predictive models from the Diabetes data set
included in the Ensembles package. Like the other functions in the
Ensembles package (numerical, classification and forecasting), logistic
uses R and Python, individual and ensemble models, regular and deep
learning. The entire process is completed automatically, once the code
below is run.

library(Ensembles)

logistic(data = diabetes, colnum = ncol(diabetes), numresamples = 5,
save_all_trained_models = “N”, how_to_handle_strings = 0,
do_you_have_new_data = “N”, remove_ensemble_correlations_greater_than =
1.00, use_parallel = “N”, train_amount = 0.60, test_amount = 0.20,
validation_amount = 0.20)

Once the code has completed running, **look in the Console for**:

Data dictionary (the type of each value in the original data set, some
initial values)

Summary tables for each of the 38 logistic models

Correlation table of the original data

Correlation table of the ensemble

Data summary (min, max, etc.)

**Look in the Plots tab for:**

Correlation plot of the numeric data (as numbers)

Correlation plot of the numeric data (as circles)

Pairwise scatter plots and histograms of the numerical data

38 ROC curves, one for each model

38 Accuracy model charts. Each dot is an individual result, the black
line is the mean of the results

38 Accuracy model charts, including train, test and validation results

Boxplots of the numeric data

Barchart of the numeric values against y (yes or no)

**Look in the Viewer tab for the summary report:**

The report is sorted with best scores on top. The columns include:

Model name for each of the 38 models

Accuracy

True positive rate (aka sensitivity)

True negative rate (aka specificity)

False positive rate (aka Type I error)

False negative rate (aka Type II error)

Positive predictive value (aka Precision)

Negative predictive value

F1 score

Area under the curve

Duration

Note that the summary report is totally searchable and it can also be
sorted with a click of a mouse.

## Basic Example: Classification analysis

This example models the location of Carseats from the Carseats dataset
in the ISLR package. There are three options for the location: Good,
Bad, and Medium, so this is a classification problem. The classification
analysis automatically builds all the models, presents all the solution
matrices, summary tables, and much more.

Similar to logistic analysis, the user does not need to set anything up,
other than making sure the target column is a factor. It does not matter
how many levels the factor has, the function will automatically make the
accurate solution matrices, tables, and all other results.

classification(data = ISLR::Carseats, colnum = 7, numresamples = 5,
do_you_have_new_data = “N”, how_to_handle_strings = 0,
save_all_trained_models = “N”, use_parallel = “N”, train_amount = 0.60,
test_amount = 0.20, validation_amount = 0.20)

Once the code has completed running, **look in the Console (use View -
Move Focus to Console if needed) for all of these results which are
automatically generated**:

Data dictionary

Summary tables

Correlation table of the original data Correlation table of the ensemble
data Data summary

**Look in the Plots tab (View - Show Plots) for all of these results
automatically generated:**

Correlation plot of the numerical data (as numbers)

Correlation plot of the numerical data (as dots with size and color
related to the correlation value) Pairwise scatter plots and histograms
of the numerical data

38 ROC curves, one for each classification modeling method used

38 Accuracy plots, one for each classification modeling method used

38 Accuracy plots that also include the results for each train, test and
validation measurement

Boxplots of the numeric data (number of boxplots will depend on the
number of numeric features in the original data set)

Numerical values against y. These barcharts show the counts of each
response against y.

**Look on the Viewer tab (View - Show Viewer) for the Summary report.**
This report includes the following for all 38 classification models:

Model name

Accuracy

True Positive Rate (aka Sensitivity)

True Negative Rate (aka Specificity)

False Positive Rate(aka Type I error)

False Negative Rate (aka Type II error)

Positive Predictive Value (aka precision)

Negative Predictive Value

F1 Score

Area Under the Curve (matches the value from the ROC plot)

Mean Duration for the Model

Note that the summary report is totally searchable, and can be sorted
with the click of a mouse.

## Basic Example: Forecasting analysis

forecasting(time_series_data = total_nonfarm,
number_of_intervals_to_forecast = 3, use_parallel = “N”, time_interval =
“M”)

This code example is from the United States Department of Labor
Statistics, and counts the total number of nonfarm employees by month.

The forecasting solution runs two independent types of analysis. One is
to model the total number (in this case of nonfarm employees) and the
other is the 1-unit change (in this case the 1-month change of the total
number of nonfarm employees).

Once the code has completed running, **look in the Console for:**

Model summaries for each of the models. The first model description is
for the more accurate model of total number, the second model
description is for the most accurate result of 1-month change.

**Click on the Plots tab for best model results:**

Value per unit of time. In our example, this shows the total number of
employees by month in the United States.

Total value (such as total number of employees) and trend (in red)

Total value (such as total number of employees) and seasonally adjusted
value

Decomposition of the data, including the basic Value, trend,
season_year, and remainder.

Anomolies in the data. The red dotted line is 1 standard deviation
around 0, the blue dotted line is 2 standard deviations around 0.

Plot of individual seasons. It’s possible to see what happens each
reporting period, such as each month.

Plot of subseasons, where each season is on its own graph

Plot of multiple lags

Best model plot of the prediction for the next intervals (in this case,
the prediction for the next three months)

Best model innovation residuals

Best model autocorrelation function

Best model histogram of residuals

Best model predicted vs actual

Best model Actual vs residuals

Starting at this point we model the one-unit change, in this case the
1-month change:

Best model plot of the forecast change

Best model autocorrelation function of change

Best model histogram of residuals

Best model, actual vs predicted

Best model, actual vs residuals

**Look on the Viewer for:**

Head (beginning) of the time series data

Tail (end) of the time series data)

Best forecast for the total number (in our example, the total number of
nonfarm employees)

Best forecast for the 1-unit change (in our example, the 1-month change
in total nonfarm employees)

Accuracy results for the total number

Accuracy results for the 1-unit change.
