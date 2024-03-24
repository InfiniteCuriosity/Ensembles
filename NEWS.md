# Ensembles 0.2.0

# Ensembles 0.1.0

* Initial CRAN submission.

The Ensembles package solves a problem for data scientists: How to make and use ensembles in the entire data science process. The only step the data scientist needs to do is provide one line of code. The Ensembles package does all of the following steps for the data scientist:

1. Sets a counter for the number of times to resample the process (as set by the user)
2. Randomizes the rows (this helps with generalization of the results)
3. Splits the data as the user requests (train, test and validation amounts)
4. Fits each of the the models to the training data
5. Makes predictions on both the test and validation sets
6. Calculates the error (root mean squared error for numerical, accuracy for logistic and classification, AICc in forecasting)
7. Uses the predictions to make an ensemble
8. Splits the ensemble into train, test and validation
9. Fits the ensemble model to the ensemble training data
10. Calculates the error (root mean squared error for numerical, accuracy for logistic and classification, AICc for forecasting)
11. Loops back and randomly re-samples the data, builds new models, then builds new ensembles, and keeps track of each result, to be used in a summary report.
12. Once the number of resamples has been reached, the system builds two types of visualizations: Exploratory data analysis, and summary visualizations.

It concludes by building a comprehensive summary report that gives details on all the models and the results run on the holdout data (test and validation).

The system has a number of advanced functions, such as allowing the user to use parallel processing, and make predictions on totally new data.

The system uses both R and Python (such as the XGBoost R package that depends on Python), individual and ensembles models, deep learning and regular learning.

The system uses 40 models for the numerical solution, 38 models for logistic analysis, 38 models for classification analysis, and 23 models for time series, all completed automatically.

Example results:

Numerical Ensemble here beats 699 college students on the Boston Housing data set by more than 90%, using ensembles, measuring only on the holdout data.
Classification Ensembles achieves 100% accuracy on the Carseats data set in the ISLR package, and 100% accuracy on the Dry Beans data set, measuring only on the holdout data.
Logistic: Ensembles achieve 100% accuracy several times over on the Pima Indians data set, measuring only on the holdout data.
Forecasting: Achieves error of 0.0003451008 forecasting total number of employees from the United States Department of Labor Statistics, measuring only on the holdout data.

The package is supported by a full website, and high quality instructional videos. The website is: https://www.dataaip.com. The videos are located: https://www.youtube.com/@russconte8281
