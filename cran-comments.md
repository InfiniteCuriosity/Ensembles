## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Depends: includes the non-default packages:
    'arm', 'brnn', 'broom', 'C50', 'caret', 'class', 'corrplot',
    'Cubist', 'doParallel', 'dplyr', 'e1071', 'fable', 'fable.prophet',
    'fabletools', 'feasts', 'gam', 'gbm', 'GGally', 'ggplot2', 'glmnet',
    'gridExtra', 'gt', 'gtExtras', 'ipred', 'kernlab', 'klaR', 'leaps',
    'MachineShop', 'magrittr', 'MASS', 'mda', 'Metrics', 'neuralnet',
    'parallel', 'pls', 'pROC', 'purrr', 'randomForest', 'reactable',
    'reactablefmtr', 'readr', 'rpart', 'scales', 'tibble', 'tidyr',
    'tree', 'tsibble', 'xgboost'
  Adding so many packages to the search path is excessive and importing
  selectively is preferable

The nature of the package is that it uses all of these packages to build the solution. Selective importing breaks the package.
