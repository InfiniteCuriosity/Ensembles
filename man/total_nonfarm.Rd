% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{total_nonfarm}
\alias{total_nonfarm}
\title{total_nonfarm—This is a time series data set. It lists the total number of nonfarm employees per month, starting in 1996. The forecasting function in the Ensembles package can be used to make time series forecasts, such as the total nonfarm employees.}
\format{
An object of class \code{data.frame} with 211 rows and 2 columns.
}
\source{
<https://data.bls.gov/timeseries/CES0000000001>
}
\usage{
total_nonfarm
}
\description{
This data set is from the Bureau of Labor Statistics. The Bureau reports the number of total nonfarm employees in the United States on the first Friday of each month (unless that day is a federal holiday).
\describe{
This data set is from: https://data.bls.gov/timeseries/CES0000000001, and is modified to include the 1-month change in total nonfarm employment.
The data consists of two columns, one to establish the date (such as Jan 2021), and the other to establish the total number of nonfarm employees that month.

\item{label}{the label for the specific time, such as 2022 Nov}
\item{Value}{the quantity for that specific time, such as 154296}
}
}
\keyword{datasets}
