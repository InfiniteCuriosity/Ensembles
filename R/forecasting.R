#' forecasting—function to perform time series analysis and return the results to the user.
#'
#' @param time_series_data a time series
#' @param train_amount The amount for the training set. For example, 0.60 is 60 percent.
#' @param number Number of time intervals to forecast. For example, 3.
#' @param time_interval user states whether the time interval is quarterly ("Q"), monthly ("M"), or weekly ("W").
#' @param use_parallel "Y" or "N" for parallel processing

#' @returns A series of summary reports and visualizations to fully describe the time series: Forecast accuracy, forecast numbers, forecast plot, innovation residuals,
#' @returns best autocorrelation function (ACF), plot of best histogram of residuals, plot of best actual vs predicted, plot of best actual vs trend
#' @returns plot of best actual vs seasonally adjusted
#'
#' @export forecasting
#'
#'
#'
#' @importFrom dplyr arrange bind_rows count filter mutate select
#' @importFrom fable ARIMA ETS MEAN NAIVE NNETAR RW SNAIVE TSLM
#' @importFrom fabletools accuracy as_tsibble augment autoplot components features report
#' @importFrom fable.prophet prophet
#' @importFrom feasts gg_tsresiduals STL
#' @importFrom ggplot2 aes facet_grid geom_line geom_hline geom_abline geom_point ggplot ggtitle guides labs scale_x_continuous scale_y_continuous theme xlab ylab
#' @importFrom gt gt tab_header fmt_number fmt_percent
#' @importFrom magrittr %>%
#' @importFrom parallel makeCluster
#' @importFrom readr read_csv
#' @importFrom stats stl AIC BIC lag
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tsibble tsibble
#' @importFrom utils tail head

forecasting <- function(time_series_data, train_amount, number, time_interval = c("Q", "M", "W"), use_parallel = c("Y", "N")) {


#' @examples
#' data <- all_employees (note this example will take a few minutes to run, the function does give updates of which model is build built)
#' forecasting(time_series_data = total_nonfarm,
#'              train_amount = 0.60,
#'              number = 3,
#'              time_interval = "M",
#'              use_parallel = "Y") # note this example takes several minutes to run on a normal computer.

  use_parallel <- 0
  no_cores <- 0

  if (use_parallel == "Y") {
    cl <- parallel::makeCluster(no_cores, type = "FORK")
    doParallel::registerDoParallel(cl)
  }

  Label <- 0
  Value <- 0
  Difference <- 0
  Date <- 0
  trend <- 0
  season_adjust <- 0
  time_series_decomposition <- 0
  table_of_value_head <- 0
  table_of_value_tail <- 0
  forecast_accuracy <- 0
  Time_Series <- 0
  Linear1 <- 0
  Linear2 <- 0
  Linear3 <- 0
  Linear4 <- 0
  Arima1 <- 0
  Arima2 <- 0
  Arima3 <- 0
  Arima4 <- 0
  Deterministic <- 0
  Stochastic <- 0
  Ets1 <- 0
  Ets2 <- 0
  Ets3 <- 0
  Ets4 <- 0
  Holt_Winters_Additive <- 0
  Holt_Winters_Multiplicative <- 0
  Holt_Winters_Damped <- 0
  Fourier1 <- 0
  Fourier2 <- 0
  Fourier3 <- 0
  Fourier4 <- 0
  Fourier5 <- 0
  Fourier6 <- 0
  Prophet_Additive <- 0
  Prophet_Multiplicative <- 0
  NeuralNet1 <- 0
  NeuralNet2 <- 0
  NeuralNet3 <- 0
  NeuralNet4 <- 0
  VAR1 <- 0
  vars <- 0
  Mean <- 0
  Naive <- 0
  SNaive <- 0
  Drift <- 0
  .model <- 0
  RMSSE <- 0
  RMSE <- 0
  .resid <- 0
  .fitted <- 0
  .mean <- 0
  .innov <- 0
  Time_Series_AIC_fit <- 0
  AICc <- 0
  MPE <- 0
  name <- 0
  value <- 0
  feat_stl <- 0


  if (time_interval == "Q") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearquarter(Label), Value = Value, Difference = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Difference) %>%
      tsibble::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }
  if (time_interval == "M") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearmonth(Label), Value = Value, Difference = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Difference) %>%
      tsibble::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }
  if (time_interval == "W") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearweek(Label), Value = Value, Difference = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Difference) %>%
      tsibble::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }


  #### Time series features for Value and Difference ####

  value_features <- time_series_data %>%
    fabletools::features(.var = Value, feasts::feat_stl)

  Difference_features <- time_series_data %>%
    fabletools::features(.var = Difference, feasts::feat_stl)

  value_trend_strength <- value_features$trend_strength
  Difference_trend_strength <- Difference_features$trend_strength

  value_season_strength_year <- value_features$seasonal_strength_year
  Difference_season_strength_year <- Difference_features$seasonal_strength_year

  value_season_peak_year <- value_features$seasonal_peak_year
  Difference_season_peak_year <- Difference_features$seasonal_peak_year

  value_season_trough_year <- value_features$seasonal_trough_year
  Difference_season_trough_year <- Difference_features$seasonal_trough_year

  value_spikiness <- value_features$spikiness
  Difference_spikiness <- Difference_features$spikiness

  value_linearity <- value_features$linearity
  Difference_linearity <- Difference_features$linearity

  value_curvature <- value_features$curvature
  Difference_curvature <- Difference_features$curvature

  Difference_features <- time_series_data %>%
    fabletools::features(.var = Difference, feasts::feat_stl)

  value_coef_hurst <- feasts::coef_hurst(x = time_series_data$Value)
  Difference_coef_hurst <- feasts::coef_hurst(x = time_series_data$Difference)

  value_spectral <- feasts::feat_spectral(x = time_series_data$Value)
  Difference_spectral <- feasts::feat_spectral(x = time_series_data$Difference)

  value_box_pierce <- feasts::box_pierce(x = time_series_data$Value)
  Difference_box_pierce <- feasts::box_pierce(x = time_series_data$Difference)

  value_ljung_box <- feasts::ljung_box(x = time_series_data$Value)
  value_ljung_box_stat <- value_ljung_box[1]
  value_ljung_box_pvalue <- value_ljung_box[2]

  Difference_ljung_box <- feasts::ljung_box(x = time_series_data$Difference)
  Difference_ljung_box_stat <- Difference_ljung_box[2]
  Difference_ljung_box_pvalue <- Difference_ljung_box[2]


  value_unitroot_kpss <- feasts::unitroot_kpss(x = time_series_data$Value)
  value_kpss_stat <- value_unitroot_kpss[1]
  value_kpss_pvalue <- value_unitroot_kpss[2]
  Difference_unitroot_kpss <- feasts::unitroot_kpss(x = time_series_data$Difference)
  Difference_kpss_stat <- Difference_unitroot_kpss[1]
  Difference_kpss_pvalue <- Difference_unitroot_kpss[2]

  value_unitroot_ndiffs <- feasts::unitroot_ndiffs(x = time_series_data$Value)
  Difference_unitroot_ndiffs <- feasts::unitroot_ndiffs(x = time_series_data$Difference)

  value_unitroot_nsdiffs <- feasts::unitroot_nsdiffs(x = time_series_data$Value)
  Difference_unitroot_nsdiffs <- feasts::unitroot_nsdiffs(x = time_series_data$Difference)

  value_longest_flat <- feasts::longest_flat_spot(x = time_series_data$Value)
  Difference_longest_flat <- feasts::longest_flat_spot(x = time_series_data$Difference)

  value_n_crossing_points <- feasts::n_crossing_points(x = time_series_data$Value)
  Difference_n_crossing_points <- feasts::n_crossing_points(x = time_series_data$Difference)

  value_longest_flat_spot <- feasts::longest_flat_spot(x = time_series_data$Value)
  Difference_longest_flat_spot <- feasts::longest_flat_spot(x = time_series_data$Difference)

  value_feat_acf <- feasts::feat_acf(x = time_series_data$Value)
  Difference_feat_acf <- feasts::feat_acf(x = time_series_data$Difference)

  options(scipen = 999)
  time_series_features <- data.frame(
    c('Trend strength', 'Seasonal strength year', 'Seasonal peak year', 'Seasonal trough year', 'Spikeiness', 'Linearity', 'Curvature', 'Coef hurst', 'Spectral',
      'Box pierce stat', 'Box pierce pvalue', 'Ljung box stat', 'Ljung box pvalue', 'KPSS stat', 'KPSS pvalue', 'Ndiffs', 'NSDiffs', 'Longest_flat', 'Crossing_points'),
    round(c(value_trend_strength, value_season_peak_year, value_season_peak_year, value_season_trough_year, value_spikiness, value_linearity, value_curvature, value_coef_hurst, value_spectral,
            value_box_pierce, value_ljung_box_stat, value_ljung_box_pvalue, value_kpss_stat, value_kpss_pvalue, value_unitroot_ndiffs, value_unitroot_nsdiffs, value_longest_flat, value_n_crossing_points), 4),
    round(c(Difference_trend_strength, Difference_season_peak_year, Difference_season_peak_year, Difference_season_trough_year, Difference_spikiness, Difference_linearity, Difference_curvature, Difference_coef_hurst, Difference_spectral,
            Difference_box_pierce, Difference_ljung_box_stat, Difference_ljung_box_pvalue, Difference_kpss_stat, Difference_kpss_pvalue, Difference_unitroot_ndiffs, Difference_unitroot_nsdiffs, Difference_longest_flat, Difference_n_crossing_points), 4)
  )
  colnames(time_series_features) <- c('Feature name', 'Value', 'Difference')
  gt::gt(time_series_features)%>%
    gt::tab_header(
      title = "Features and their measures"
    )

  time_series_features <- data.frame(
    c('Trend strength', 'Seasonal strength year', 'Seasonal peak year', 'Seasonal trough year', 'Spikeiness', 'Linearity', 'Curvature', 'Coef hurst', 'Spectral',
      'Box pierce stat', 'Box pierce pvalue', 'Ljung box stat', 'Ljung box pvalue', 'KPSS stat', 'KPSS pvalue', 'Ndiffs', 'NSDiffs', 'Longest_flat', 'Crossing_points'),
    round(c(value_trend_strength, value_season_peak_year, value_season_peak_year, value_season_trough_year, value_spikiness, value_linearity, value_curvature, value_coef_hurst, value_spectral,
            value_box_pierce, value_ljung_box_stat, value_ljung_box_pvalue, value_kpss_stat, value_kpss_pvalue, value_unitroot_ndiffs, value_unitroot_nsdiffs, value_longest_flat, value_n_crossing_points), 4),
    round(c(Difference_trend_strength, Difference_season_peak_year, Difference_season_peak_year, Difference_season_trough_year, Difference_spikiness, Difference_linearity, Difference_curvature, Difference_coef_hurst, Difference_spectral,
            Difference_box_pierce, Difference_ljung_box_stat, Difference_ljung_box_pvalue, Difference_kpss_stat, Difference_kpss_pvalue, Difference_unitroot_ndiffs, Difference_unitroot_nsdiffs, Difference_longest_flat, Difference_n_crossing_points), 4)
  )
  colnames(time_series_features) <- c('Feature name', 'Value', 'Difference')
  gt::gt(time_series_features)%>%
    gt::tab_header(
      title = "Features and their measures"
    )


  #######

  all_time_series_features <- gt::gt(time_series_features) %>%
    gt::tab_header(
      title = "Features of the time series")


  #### Quartiles and Quintiles ####
  time_series_quartiles <- time_series_data %>%
    fabletools::features(Value, quantile, prob = seq(0, 1, 0.25))

  time_series_difference_quartiles <- time_series_data %>%
    fabletools::features(Difference, quantile, prob = seq(0, 1, 0.25))


  #########

  seasonal_plots <- time_series_data %>%
    feasts::gg_season(y = Value, labels = "both") +
    labs(title = "Seasonal plot")

  #########

  #########

  lag_plots <- time_series_data %>%
    feasts::gg_lag(y = Value) +
    labs(title = "Lag plots")


  # Baseline forecasts—Can you beat these?
  Mean_value <- mean(time_series_data$Value)
  SD_Value <- sd(time_series_data$Value)
  Mean_value_most_recent <- mean(head(time_series_data$Value, 3))
  SD_value_most_recent <- sd(head(time_series_data$Value, 3))

  Mean_Difference <- mean(time_series_data$Difference)
  SD_Difference <- sd(time_series_data$Difference)
  Mean_Difference_most_recent <- mean(head(time_series_data$Difference, 3))
  SD_Difference_most_recent <- sd(head(time_series_data$Difference, 3))

  Min_value <- min(time_series_data$Value)
  Min_Difference <- min(time_series_data$Difference)

  Max_value <- max(time_series_data$Value)
  Max_Difference <- max(time_series_data$Difference)

  value_mean <- mean(time_series_data$Value)
  Difference_mean <- mean(time_series_data$Difference)
  value_sd <- sd(time_series_data$Value)
  Difference_sd <- sd(time_series_data$Difference)
  baseline_full <- gt::gt(data.frame('Baseline' = c('Value', 'Difference'),
                                     'Mean' = c(value_mean, Difference_mean),
                                     'Std.Dev' = c(value_sd, Difference_sd)))%>%
    gt::tab_header(
      title = "Results from the entire time series"
    )

  baseline_difference <- gt::gt(data.frame('Baseline' = c("Value", "Difference"),
                                           'Mean' = c(Mean_value_most_recent, Mean_Difference_most_recent),
                                           "Std_deviation" = c(SD_value_most_recent, SD_Difference_most_recent)))%>%
    gt::tab_header(
      title = "Results from most recent three time intervals"
    )


  # <----- Plot of Value ----------------------------------------------> ####
  plot_of_value <- time_series_data %>%
    ggplot2::ggplot(aes(x = Date, y = Value)) +
    ggplot2::geom_line(aes(x = Date, y = Value)) +
    ggplot2::geom_point(aes(x = Date, y = Value)) +
    ggplot2::geom_point(aes(x = Date, y = Value)) +
    ggplot2::labs(title = "Value per time_interval of time")


  # -------------------------- 3. Exploratory Data Analysis -------------------------- #

  # Preview the top of the data set
  head_of_time_series <- gt::gt(head(time_series_data, n = 10)) %>%
    gt::tab_header(
      title = "Head of the time series data set"
    )

  tail_of_time_series <- gt::gt(tail(time_series_data, n = 10)) %>%
    gt::tab_header(
      title = "Tail of the time series data set"
    )


  # Raw data
  basic_graph <- time_series_data %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, colour = name)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::facet_grid(name ~ ., scales = "free_y") +
    ggplot2::guides(colour = "none") +
    ggplot2::labs(y="Difference") +
    ggplot2::ggtitle("1-time_interval Difference (top) and total value (botton)")

  # Seasonally adjusted values and trend
  Time_Series_dcmp <- time_series_data %>%
    fabletools::model(stl = STL(Value))

  full_time_series <- fabletools::components(Time_Series_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date:season_adjust) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::facet_grid(~name ~ ., scales = "free_y") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("Value for the full data set")

  full_time_series_2 <- fabletools::components(Time_Series_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, Value) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::facet_grid(~name ~ ., scales = "free_y") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("Value for full data")

  value_and_trend <- fabletools::components(Time_Series_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, Value, trend) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("Value and Trend for full data")

  value_and_seasonally_adjusted <- fabletools::components(Time_Series_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, Value, season_adjust) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("Value and seasonally adjusted for full data")

  # Seasonally adjusted values and trend for the 1-unit Difference of the number of nonfarm workers
  Time_Series_Difference_dcmp <- time_series_data %>%
    fabletools::model(stl = STL(Difference))

  difference_plot1 <- fabletools::components(Time_Series_Difference_dcmp) %>% drop_na() %>%
    dplyr::select(Date:season_adjust) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::facet_grid(~name ~ ., scales = "free_y") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("The 1-time_interval Difference giving the remainder, seasonally adjusted value, annually adjusted value, and trend")

  difference_plot2 <- fabletools::components(Time_Series_Difference_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, Difference) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("1-time unit Difference for the data")

  difference_plot3 <- fabletools::components(Time_Series_Difference_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, trend) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("1-time unit trend for the Difference data")

  difference_plot4 <- fabletools::components(Time_Series_Difference_dcmp) %>% tidyr::drop_na() %>%
    dplyr::select(Date, season_adjust) %>%
    tsibble::as_tsibble() %>%
    tidyr::pivot_longer(-Date) %>%
    ggplot2::ggplot(aes(Date, value, color = name)) +
    ggplot2::geom_line(linewidth =1) +
    ggplot2::geom_point(aes(x = Date, y = value)) +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle("1-time unit season adjusted for the Difference data")

  # Anomalies
  df_anomalies <- fabletools::components(Time_Series_dcmp) %>% drop_na()

  remainder = fabletools::components(Time_Series_dcmp)$remainder

  remainder1 <- sd(remainder)

  Print_Time_Series_Anomalies <- ggplot2::ggplot(data = df_anomalies, aes(x = Date, y = remainder)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(remainder1, -remainder1), linetype = 'dashed', color = 'blue') +
    ggplot2::geom_hline(yintercept = c(2*remainder1, -2*remainder1), linetype = 'dashed', color = 'red') +
    ggplot2::geom_hline(yintercept = 0, color = 'black') +
    ggplot2::labs(title = "Anomalies in total data \nblue line = 1 standard deviation +/- 0, red line = 2 standard deviations +/- 0")

  Print_Time_Series_Anomalies

  # Anomalies for 1-unit Difference
  df_anomalies_Difference <- fabletools::components(Time_Series_Difference_dcmp) %>% drop_na()

  remainder_Difference = fabletools::components(Time_Series_Difference_dcmp)$remainder

  remainder2 <- sd(remainder_Difference)

  Print_Time_Series_Difference_Anomalies <- ggplot2::ggplot(data = df_anomalies_Difference, aes(x = Date, y = remainder)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(remainder1, -remainder1), linetype = 'dashed', color = 'blue') +
    ggplot2::geom_hline(yintercept = c(2*remainder1, -2*remainder1), linetype = 'dashed', color = 'red') +
    ggplot2::geom_hline(yintercept = 0, color = 'black') +
    ggplot2::labs(title = "Anomalies in 1-unit Difference in the data \nBlue line = 1 standard deviation +/- 0, Red line = 2 standard deviations +/- 0")

  Print_Time_Series_Difference_Anomalies


  # GGally::ggpairs
  ggpairs1 <- time_series_data[, 2:ncol(time_series_data)] %>%
    GGally::ggpairs(title = "Pairwise plots of time series and 1-time_interval Difference")

  # regular subseries
  ggpairs2 <- time_series_data %>%
    feasts::gg_subseries(y = Value) +
    ggplot2::labs(title = "Time series subseries")


  # ------------------------ 4. Split data into train (60%) and test (40%) --------------------------- #

  time_series_train <- time_series_data[1:round(train_amount*(nrow(time_series_data))),]
  time_series_test <- time_series_data[(round(train_amount*(nrow(time_series_data))) +1):nrow(time_series_data),]

  # ------------------------ 5. Individual Model building --------------------------------------------------------------------------------#

  print("Working on ARIMA models")

  #<----- ARIMA 1 -------------------->

  Arima1_start <- Sys.time()
  Arima1_model = fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
  Arima1_end <- Sys.time()
  Arima1_duration <- Arima1_end - Arima1_start

  Arima1_test_error <- time_series_train %>%
    fabletools::model(Arima1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Arima1_RMSE <- Arima1_test_error$RMSE
  Arima1_Mean_Error <- Arima1_test_error$ME
  Arima1_Mean_Absolute_Error <- Arima1_test_error$MAE
  Arima1_Mean_Percentage_Error <- Arima1_test_error$MPE
  Arima1_Autocorrelation_Function <- Arima1_test_error$ACF1

  Arima1_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 1 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima1_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Arima1_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Arima 1 Autocorrelation function')

  Arima1_histogram_of_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 1 model histogram of residuals")

  Arima1_predictions <- time_series_test %>%
    fabletools::model(
      Arima1_model,
    ) %>%
    fabletools::forecast(h = number)

  Arima1_plot_of_forecast <- time_series_data %>%
    fabletools::model(Arima1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Arima1 forecast, \nNumber of time intervals = ", number))

  Arima1_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Arima1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 1 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima1_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Arima1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 1 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Arima1_prediction_model <- Arima1_predictions[1]
  Arima1_prediction_date<- Arima1_predictions[2]
  Arima1_prediction_range <- Arima1_predictions[3]
  Arima1_prediction_mean <-Arima1_predictions[4]

  #<----- ARIMA 2 -------------------->

  Arima2_start <- Sys.time()
  Arima2_model <- fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
  Arima2_end <- Sys.time()
  Arima2_duration <- Arima2_end - Arima2_start

  Arima2_test_error <- time_series_train %>%
    fabletools::model(Arima2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Arima2_RMSE <- Arima2_test_error$RMSE
  Arima2_Mean_Error <- Arima2_test_error$ME
  Arima2_Mean_Absolute_Error <- Arima2_test_error$MAE
  Arima2_Mean_Percentage_Error <- Arima2_test_error$MPE
  Arima2_Autocorrelation_Function <- Arima2_test_error$ACF1

  Arima2_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 2 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima2_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Arima2_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Arima 2 Autocorrelation function')

  Arima2_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 2 model histogram of residuals")

  Arima2_predictions <- time_series_test %>%
    fabletools::model(
      Arima2_model,
    ) %>%
    fabletools::forecast(h = number)

  Arima2_plot_of_forecast <- time_series_data %>%
    fabletools::model(Arima2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Arima2 forecast, \nNumber of time intervals = ", number))

  Arima2_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Arima2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 2 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima2_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Arima2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 2 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Arima2_prediction_model <- Arima2_predictions[1]
  Arima2_prediction_date<- Arima2_predictions[2]
  Arima2_prediction_range <- Arima2_predictions[3]
  Arima2_prediction_mean <-Arima2_predictions[4]

  #<----- ARIMA 3 -------------------->

  Arima3_start <- Sys.time()
  Arima3_model <- fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
  Arima3_end <- Sys.time()
  Arima3_duration <- Arima3_end - Arima3_start

  Arima3_test_error <- time_series_train %>%
    fabletools::model(Arima3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Arima3_RMSE <- Arima3_test_error$RMSE
  Arima3_Mean_Error <- Arima3_test_error$ME
  Arima3_Mean_Absolute_Error <- Arima3_test_error$MAE
  Arima3_Mean_Percentage_Error <- Arima3_test_error$MPE
  Arima3_Autocorrelation_Function <- Arima3_test_error$ACF1

  Arima3_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 3 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima3_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Arima3_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Arima 3 Autocorrelation function')

  Arima3_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 3 model histogram of residuals")

  Arima3_predictions <- time_series_test %>%
    fabletools::model(
      Arima3_model,
    ) %>%
    fabletools::forecast(h = number)

  Arima3_plot_of_forecast <- time_series_data %>%
    fabletools::model(Arima3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Arima3 forecast, \nNumber of time intervals = ", number))

  Arima3_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Arima3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 3 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima3_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Arima3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 3 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Arima3_prediction_model <- Arima3_predictions[1]
  Arima3_prediction_date<- Arima3_predictions[2]
  Arima3_prediction_range <- Arima3_predictions[3]
  Arima3_prediction_mean <-Arima3_predictions[4]

  #<----- ARIMA 4 -------------------->

  Arima4_start <- Sys.time()
  Arima4_model <- fable::ARIMA(Value)
  Arima4_end <- Sys.time()
  Arima4_duration <- Arima4_end - Arima4_start

  Arima4_test_error <- time_series_train %>%
    fabletools::model(Arima4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Arima4_RMSE <- Arima4_test_error$RMSE
  Arima4_Mean_Error <- Arima4_test_error$ME
  Arima4_Mean_Absolute_Error <- Arima4_test_error$MAE
  Arima4_Mean_Percentage_Error <- Arima4_test_error$MPE
  Arima4_Autocorrelation_Function <- Arima4_test_error$ACF1

  Arima4_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 4 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima4_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Arima4_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Arima 4 Autocorrelation function')

  Arima4_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Arima4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Arima 4 model histogram of residuals")

  Arima4_predictions <- time_series_test %>%
    fabletools::model(
      Arima4_model,
    ) %>%
    fabletools::forecast(h = number)

  Arima4_plot_of_forecast <- time_series_data %>%
    fabletools::model(Arima4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Arima4 forecast, \nNumber of time intervals = ", number))

  Arima4_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Arima4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 4 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Arima4_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Arima4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Arima 4 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Arima4_prediction_model <- Arima4_predictions[1]
  Arima4_prediction_date<- Arima4_predictions[2]
  Arima4_prediction_range <- Arima4_predictions[3]
  Arima4_prediction_mean <-Arima4_predictions[4]

  #<----- Deterministic -------------------->

  Deterministic_start <- Sys.time()
  Deterministic_model <- fable::ARIMA(Value ~  1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
  Deterministic_end <- Sys.time()
  Deterministic_duration <- Deterministic_end - Deterministic_start

  Deterministic_test_error <- time_series_train %>%
    fabletools::model(Deterministic_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Deterministic_RMSE <- Deterministic_test_error$RMSE
  Deterministic_Mean_Error <- Deterministic_test_error$ME
  Deterministic_Mean_Absolute_Error <- Deterministic_test_error$MAE
  Deterministic_Mean_Percentage_Error <- Deterministic_test_error$MPE

  Deterministic_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Deterministic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Deterministic model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Deterministic_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Deterministic_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Deterministic Autocorrelation function')

  Deterministic_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Deterministic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Deterministic model histogram of residuals")

  Deterministic_predictions <- time_series_test %>%
    fabletools::model(
      Deterministic_model
    ) %>%
    fabletools::forecast(h = number)

  Deterministic_plot_of_forecast <- time_series_data %>%
    fabletools::model(Deterministic_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Deterministic forecast, \nNumber of time intervals = ", number))

  Deterministic_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Deterministic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Deterministic model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Deterministic_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Deterministic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Deterministic model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Deterministic_prediction_model <- Deterministic_predictions[1]
  Deterministic_prediction_date<- Deterministic_predictions[2]
  Deterministic_prediction_range <- Deterministic_predictions[3]
  Deterministic_prediction_mean <-Deterministic_predictions[4]

  #<----- Drift -------------------->

  print("Working on Drift model")

  Drift_start <- Sys.time()
  Drift_model <- fable::SNAIVE(Value ~ drift())
  Drift_end <- Sys.time()
  Drift_duration <- Drift_end - Drift_start

  Drift_test_error <- time_series_train %>%
    fabletools::model(Drift_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Drift_RMSE <- Drift_test_error$RMSE
  Drift_Mean_Error <- Drift_test_error$ME
  Drift_Mean_Absolute_Error <- Drift_test_error$MAE
  Drift_Mean_Percentage_Error <- Drift_test_error$MPE

  Drift_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Drift_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Drift model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Drift_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Drift_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Drift Autocorrelation function')

  Drift_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Drift_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Drift model histogram of residuals")

  Drift_predictions <- time_series_test %>%
    fabletools::model(
      Drift_model,
    ) %>%
    fabletools::forecast(h = number)

  Drift_plot_of_forecast <- time_series_data %>%
    fabletools::model(Drift_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Drift forecast, \nNumber of time intervals = ", number))

  Drift_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Drift_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Drift model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Drift_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Drift_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Drift model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Drift_prediction_model <- Drift_predictions[1]
  Drift_prediction_date <- Drift_predictions[2]
  Drift_prediction_range <- Drift_predictions[3]
  Drift_prediction_mean <-Drift_predictions[4]

  #<----- Ensemble-------------------->

  print("Working on the Ensemble of time series models")

  Ensembles_start <- Sys.time()
  Ensembles_model <- time_series_train %>%
    fabletools::model(
      Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
        fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
        fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
        fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
        fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
        fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
        fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
    )
  Ensembles_end <- Sys.time()
  Ensembles_duration <- Ensembles_end - Ensembles_start

  Ensembles_predictions <- Ensembles_model %>%
    fabletools::forecast(h = number)

  Ensembles_test_error <- Ensembles_predictions %>%
    fabletools::accuracy(time_series_data)

  Ensembles_RMSE <- Ensembles_test_error$RMSE
  Ensembles_Mean_Error <- Ensembles_test_error$ME
  Ensembles_Mean_Absolute_Error <- Ensembles_test_error$MAE
  Ensembles_Mean_Percentage_Error <- Ensembles_test_error$MPE
  Ensembles_Autocorrelation_Function <- Ensembles_test_error$ACF1

  Ensembles_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Ensembles model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ensemble_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Ensemble Autocorrelation function')

  Ensemble_histogram_of_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ensemble model histogram of residuals")

  Ensemble_plot_of_forecast <- time_series_data %>%
    fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
      fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
      fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
      fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
      fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
      fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
      fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
    )  %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Ensembles forecast, \nNumber of time intervals = ", number))

  Ensemble_predicted_vs_actual <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ensembles \nPredicted vs actual")

  Ensemble_predicted_vs_residual <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ensembles actual vs residuals")

  Ensemble_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ensemble = ((fable::TSLM(Value ~ season() + trend()) + fable::TSLM(Value) + fable::TSLM(Value ~ season()) + fable::TSLM(Value ~ trend()) +
    fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE) + fable::ARIMA(Value)) + fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE) +
    fable::ETS(Value ~ season() + trend()) + fable::ETS(Value ~ trend()) + fable::ETS(Value ~ season()) + fable::ETS(Value) +
    fable::ETS(Value ~ error("A") + trend("A") + season("A")) + fable::ETS(Value ~ error("M") + trend("A") + season("M")) + fable::ETS(Value ~ error("M") + trend("Ad") + season("M")) +
    fable::MEAN(Value) + fable::NAIVE(Value) + fable::SNAIVE(Value) + fable::SNAIVE(Value ~ drift()) + fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")) +
    fable.prophet::prophet(Value ~ season(period = 12, type = "additive")) + fable::NNETAR(Value ~ season() + trend()) + fable::NNETAR(Value ~ trend()) + fable::NNETAR(Value ~ season()) + fable::NNETAR(Value))/26
  )) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ensembles innovation residuals")

  #<----- ETS1 -------------------->

  print("Working on ETS (Error Tend and Seasonality or Exponential Smoothing) models")

  Ets1_start <- Sys.time()
  Ets1_model <- fable::ETS(Value ~ season() + trend())
  Ets1_end <- Sys.time()
  Ets1_duration <- Ets1_end - Ets1_start

  Ets1_test_error <- time_series_train %>%
    fabletools::model(Ets1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Ets1_RMSE <- Ets1_test_error$RMSE
  Ets1_Mean_Error <- Ets1_test_error$ME
  Ets1_Mean_Absolute_Error <- Ets1_test_error$MAE
  Ets1_Mean_Percentage_Error <- Ets1_test_error$MPE

  Ets1_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets1 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets1_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Ets1_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Ets1 Autocorrelation function')

  Ets1_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets1 model histogram of residuals")

  Ets1_predictions <- time_series_test %>%
    fabletools::model(
      Ets1_model,
    ) %>%
    fabletools::forecast(h = number)

  Ets1_plot_of_forecast <- time_series_data %>%
    fabletools::model(Ets1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Ets1 forecast, \nNumber of time intervals = ", number))

  Ets1_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Ets1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets1 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets1_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Ets1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets1 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Ets1_prediction_model <- Ets1_predictions[1]
  Ets1_prediction_date<- Ets1_predictions[2]
  Ets1_prediction_range <- Ets1_predictions[3]
  Ets1_prediction_mean <-Ets1_predictions[4]

  #<----- ETS2 -------------------->

  Ets2_start <- Sys.time()
  Ets2_model <- fable::ETS(Value ~ trend())
  Ets2_end <- Sys.time()
  Ets2_duration <- Ets2_end - Ets2_start

  Ets2_test_error <- time_series_train %>%
    fabletools::model(Ets2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Ets2_RMSE <- Ets2_test_error$RMSE
  Ets2_Mean_Error <- Ets2_test_error$ME
  Ets2_Mean_Absolute_Error <- Ets2_test_error$MAE
  Ets2_Mean_Percentage_Error <- Ets2_test_error$MPE

  Ets2_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets2 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets2_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Ets2_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Ets2 Autocorrelation function')

  Ets2_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets2 model histogram of residuals")

  Ets2_predictions <- time_series_test %>%
    fabletools::model(
      Ets2_model,
    ) %>%
    fabletools::forecast(h = number)

  Ets2_plot_of_forecast <- time_series_data %>%
    fabletools::model(Ets2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Ets2 forecast, \nNumber of time intervals = ", number))

  Ets2_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Ets2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets2 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets2_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Ets2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets2 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Ets2_prediction_model <- Ets2_predictions[1]
  Ets2_prediction_date<- Ets2_predictions[2]
  Ets2_prediction_range <- Ets2_predictions[3]
  Ets2_prediction_mean <-Ets2_predictions[4]

  #<----- ETS3 -------------------->

  Ets3_start <- Sys.time()
  Ets3_model <- fable::ETS(Value ~ season())
  Ets3_end <- Sys.time()
  Ets3_duration <- Ets3_end - Ets3_start

  Ets3_test_error <- time_series_train %>%
    fabletools::model(Ets3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Ets3_RMSE <- Ets3_test_error$RMSE
  Ets3_Mean_Error <- Ets3_test_error$ME
  Ets3_Mean_Absolute_Error <- Ets3_test_error$MAE
  Ets3_Mean_Percentage_Error <- Ets3_test_error$MPE

  Ets3_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets3 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets3_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Ets3_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Ets3 Autocorrelation function')

  Ets3_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets3 model histogram of residuals")

  Ets3_predictions <- time_series_test %>%
    fabletools::model(
      Ets3_model,
    ) %>%
    fabletools::forecast(h = number)

  Ets3_plot_of_forecast <- time_series_data %>%
    fabletools::model(Ets3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Ets3 forecast, \nNumber of time intervals = ", number))

  Ets3_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Ets3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets3 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets3_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Ets3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets3 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Ets3_prediction_model <- Ets3_predictions[1]
  Ets3_prediction_date<- Ets3_predictions[2]
  Ets3_prediction_range <- Ets3_predictions[3]
  Ets3_prediction_mean <-Ets3_predictions[4]

  #<----- ETS4 -------------------->

  Ets4_start <- Sys.time()
  Ets4_model <- fable::ETS(Value)
  Ets4_end <- Sys.time()
  Ets4_duration <- Ets4_end - Ets4_start

  Ets4_test_error <- time_series_train %>%
    fabletools::model(Ets4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Ets4_RMSE <- Ets4_test_error$RMSE
  Ets4_Mean_Error <- Ets4_test_error$ME
  Ets4_Mean_Absolute_Error <- Ets4_test_error$MAE
  Ets4_Mean_Percentage_Error <- Ets4_test_error$MPE

  Ets4_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets4 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets4_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Ets4_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Ets4 Autocorrelation function')

  Ets4_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Ets4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Ets4 model histogram of residuals")

  Ets4_predictions <- time_series_test %>%
    fabletools::model(
      Ets4_model,
    ) %>%
    fabletools::forecast(h = number)

  Ets4_plot_of_forecast <- time_series_data %>%
    fabletools::model(Ets4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Ets4 forecast, \nNumber of time intervals = ", number))

  Ets4_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Ets4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets4 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Ets4_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Ets4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Ets4 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Ets4_prediction_model <- Ets4_predictions[1]
  Ets4_prediction_date<- Ets4_predictions[2]
  Ets4_prediction_range <- Ets4_predictions[3]
  Ets4_prediction_mean <-Ets4_predictions[4]


  #<----- Holt_Winters_Additive -------------------->

  print("Working on the Holt-Winters models")

  Holt_Winters_Additive_start <- Sys.time()
  Holt_Winters_Additive_model <- fable::ETS(Value ~ error("A") + trend("A") + season("A"))
  Holt_Winters_Additive_end <- Sys.time()
  Holt_Winters_Additive_duration <- Holt_Winters_Additive_end - Holt_Winters_Additive_start

  Holt_Winters_Additive_test_error <- time_series_train %>%
    fabletools::model(Holt_Winters_Additive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Holt_Winters_Additive_RMSE <- Holt_Winters_Additive_test_error$RMSE
  Holt_Winters_Additive_Mean_Error <- Holt_Winters_Additive_test_error$ME
  Holt_Winters_Additive_Mean_Absolute_Error <- Holt_Winters_Additive_test_error$MAE
  Holt_Winters_Additive_Mean_Percentage_Error <- Holt_Winters_Additive_test_error$MPE

  Holt_Winters_Additive_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt_Winters_Additive model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Additive_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Additive_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Holt_Winters_Additive Autocorrelation function')

  Holt_Winters_Additive_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt_Winters_Additive model histogram of residuals")

  Holt_Winters_Additive_predictions <- time_series_test %>%
    fabletools::model(
      Holt_Winters_Additive_model,
    ) %>%
    fabletools::forecast(h = number)

  Holt_Winters_Additive_plot_of_forecast <- time_series_data %>%
    fabletools::model(Holt_Winters_Additive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Holt_Winters_Additive forecast, \nNumber of time intervals = ", number))

  Holt_Winters_Additive_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt_Winters_Additive model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Additive_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt_Winters_Additive model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Holt_Winters_Additive_prediction_model <- Holt_Winters_Additive_predictions[1]
  Holt_Winters_Additive_prediction_date <- Holt_Winters_Additive_predictions[2]
  Holt_Winters_Additive_prediction_range <- Holt_Winters_Additive_predictions[3]
  Holt_Winters_Additive_prediction_mean <-Holt_Winters_Additive_predictions[4]

  #<----- Holt Winters Damped -------------------->

  Holt_Winters_Damped_start <- Sys.time()
  Holt_Winters_Damped_model <- fable::ETS(Value ~ error("M") + trend("Ad") + season("M"))
  Holt_Winters_Damped_end <- Sys.time()
  Holt_Winters_Damped_duration <- Holt_Winters_Damped_end - Holt_Winters_Damped_start

  Holt_Winters_Damped_test_error <- time_series_train %>%
    fabletools::model(Holt_Winters_Damped_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Holt_Winters_Damped_RMSE <- Holt_Winters_Damped_test_error$RMSE
  Holt_Winters_Damped_Mean_Error <- Holt_Winters_Damped_test_error$ME
  Holt_Winters_Damped_Mean_Absolute_Error <- Holt_Winters_Damped_test_error$MAE
  Holt_Winters_Damped_Mean_Percentage_Error <- Holt_Winters_Damped_test_error$MPE

  Holt_Winters_Damped_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Damped_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt Winters Damped model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Damped_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Damped_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Holt Winters Damped Autocorrelation function')

  Holt_Winters_Damped_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Damped_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt Winters Damped model histogram of residuals")

  Holt_Winters_Damped_predictions <- time_series_test %>%
    fabletools::model(
      Holt_Winters_Damped_model,
    ) %>%
    fabletools::forecast(h = number)

  Holt_Winters_Damped_plot_of_forecast <- time_series_data %>%
    fabletools::model(Holt_Winters_Damped_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Holt_Winters_Damped forecast, \nNumber of time intervals = ", number))

  Holt_Winters_Damped_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Damped_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt Winters Damped model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Damped_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Damped_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt Winters Damped model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Holt_Winters_Damped_prediction_model <- Holt_Winters_Damped_predictions[1]
  Holt_Winters_Damped_prediction_date <- Holt_Winters_Damped_predictions[2]
  Holt_Winters_Damped_prediction_range <- Holt_Winters_Damped_predictions[3]
  Holt_Winters_Damped_prediction_mean <-Holt_Winters_Damped_predictions[4]


  #<----- Holt Winters Multiplicative -------------------->

  Holt_Winters_Multiplicative_start <- Sys.time()
  Holt_Winters_Multiplicative_model <- fable::ETS(Value ~ error("M") + trend("A") + season("M"))
  Holt_Winters_Multiplicative_end <- Sys.time()
  Holt_Winters_Multiplicative_duration <- Holt_Winters_Multiplicative_end - Holt_Winters_Multiplicative_start

  Holt_Winters_Multiplicative_test_error <- time_series_train %>%
    fabletools::model(Holt_Winters_Multiplicative_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Holt_Winters_Multiplicative_RMSE <- Holt_Winters_Multiplicative_test_error$RMSE
  Holt_Winters_Multiplicative_Mean_Error <- Holt_Winters_Multiplicative_test_error$ME
  Holt_Winters_Multiplicative_Mean_Absolute_Error <- Holt_Winters_Multiplicative_test_error$MAE
  Holt_Winters_Multiplicative_Mean_Percentage_Error <- Holt_Winters_Multiplicative_test_error$MPE

  Holt_Winters_Multiplicative_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt Winters Multiplicative model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Multiplicative_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Multiplicative_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Holt Winters Multiplicative Autocorrelation function')

  Holt_Winters_Multiplicative_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Holt_Winters_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Holt Winters Multiplicative model histogram of residuals")

  Holt_Winters_Multiplicative_predictions <- time_series_test %>%
    fabletools::model(
      Holt_Winters_Multiplicative_model,
    ) %>%
    fabletools::forecast(h = number)

  Holt_Winters_Multiplicative_plot_of_forecast <- time_series_data %>%
    fabletools::model(Holt_Winters_Multiplicative_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Holt_Winters_Multiplicative forecast, \nNumber of time intervals = ", number))

  Holt_Winters_Multiplicative_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt Winters Multiplicative model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Holt_Winters_Multiplicative_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Holt_Winters_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Holt Winters Multiplicative model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Holt_Winters_Multiplicative_prediction_model <- Holt_Winters_Multiplicative_predictions[1]
  Holt_Winters_Multiplicative_prediction_date <- Holt_Winters_Multiplicative_predictions[2]
  Holt_Winters_Multiplicative_prediction_range <- Holt_Winters_Multiplicative_predictions[3]
  Holt_Winters_Multiplicative_prediction_mean <-Holt_Winters_Multiplicative_predictions[4]


  #<----- Linear model 1 -------------------->

  print("Working on the Linear models")

  Linear1_start <- Sys.time()
  Linear1_model = fable::TSLM(Value ~ season() + trend())
  Linear1_end <- Sys.time()
  Linear1_duration <- Linear1_end - Linear1_start

  Linear1_test_error <- time_series_train %>%
    fabletools::model(fable::TSLM(Value ~ season() + trend())) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Linear1_RMSE <- Linear1_test_error$RMSE
  Linear1_Mean_Error <- Linear1_test_error$ME
  Linear1_Mean_Absolute_Error <- Linear1_test_error$MAE
  Linear1_Mean_Percentage_Error <- Linear1_test_error$MPE

  linear1_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 1 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear1_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Linear1_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Linear 1 Autocorrelation function')

  linear1_histogram_of_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 1 model histogram of residuals")

  linear1_predictions <- time_series_test %>%
    fabletools::model(
      Linear1_model,
    ) %>%
    fabletools::forecast(h = number)

  Linear1_plot_of_forecast <- time_series_data %>%
    fabletools::model(Linear1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Linear1 forecast, \nNumber of time intervals = ", number))

  linear1_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Linear1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 1 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear1_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Linear1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 1 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  linear1_prediction_model <- linear1_predictions[1]
  linear1_prediction_date<- linear1_predictions[2]
  linear1_prediction_range <- linear1_predictions[3]
  linear1_prediction_mean <-linear1_predictions[4]

  #<----- Linear model 2 -------------------->

  Linear2_start <- Sys.time()
  Linear2_model = fable::TSLM(Value)
  Linear2_end <- Sys.time()
  Linear2_duration <- Linear2_end - Linear2_start

  Linear2_test_error <- time_series_train %>%
    fabletools::model(Linear2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Linear2_RMSE <- Linear2_test_error$RMSE
  Linear2_Mean_Error <- Linear2_test_error$ME
  Linear2_Mean_Absolute_Error <- Linear2_test_error$MAE
  Linear2_Mean_Percentage_Error <- Linear2_test_error$MPE
  Linear2_Autocorrelation_Function <- Linear2_test_error$ACF1

  linear2_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 2 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear2_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Linear2_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Linear 2 Autocorrelation function')

  linear2_histogram_of_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 2 model histogram of residuals")

  linear2_predictions <- time_series_test %>%
    fabletools::model(
      Linear2_model,
    ) %>%
    fabletools::forecast(h = number)

  Linear2_plot_of_forecast <- time_series_data %>%
    fabletools::model(Linear2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Linear2 forecast, \nNumber of time intervals = ", number))

  linear2_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Linear2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 2 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear2_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Linear2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 2 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  linear2_prediction_model <- linear2_predictions[1]
  linear2_prediction_date<- linear2_predictions[2]
  linear2_prediction_range <- linear2_predictions[3]
  linear2_prediction_mean <-linear2_predictions[4]


  #<----- Linear model 3 -------------------->

  Linear3_start <- Sys.time()
  Linear3_model = fable::TSLM(Value ~ season())
  Linear3_end <- Sys.time()
  Linear3_duration <- Linear3_end - Linear3_start

  Linear3_test_error <- time_series_train %>%
    fabletools::model(Linear3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Linear3_RMSE <- Linear3_test_error$RMSE
  Linear3_Mean_Error <- Linear3_test_error$ME
  Linear3_Mean_Absolute_Error <- Linear3_test_error$MAE
  Linear3_Mean_Percentage_Error <- Linear3_test_error$MPE
  Linear3_Autocorrelation_Function <- Linear3_test_error$ACF1

  linear3_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 3 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear3_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Linear3_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Linear 3 Autocorrelation function')

  linear3_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 3 model histogram of residuals")

  linear3_predictions <- time_series_test %>%
    fabletools::model(
      Linear3_model,
    ) %>%
    fabletools::forecast(h = number)

  Linear3_plot_of_forecast <- time_series_data %>%
    fabletools::model(Linear3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Linear3 forecast, \nNumber of time intervals = ", number))

  linear3_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Linear3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 3 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear3_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Linear3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 3 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  linear3_prediction_model <- linear3_predictions[1]
  linear3_prediction_date<- linear3_predictions[2]
  linear3_prediction_range <- linear3_predictions[3]
  linear3_prediction_mean <-linear3_predictions[4]


  #<----- Linear model 4 -------------------->

  Linear4_start <- Sys.time()
  Linear4_model = Linear4 = fable::TSLM(Value ~ trend())
  Linear4_end <- Sys.time()
  Linear4_duration <- Linear4_end - Linear4_start

  Linear4_test_error <- time_series_train %>%
    fabletools::model(Linear4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Linear4_RMSE <- Linear4_test_error$RMSE
  Linear4_Mean_Error <- Linear4_test_error$ME
  Linear4_Mean_Absolute_Error <- Linear4_test_error$MAE
  Linear4_Mean_Percentage_Error <- Linear4_test_error$MPE
  Linear4_Autocorrelation_Function <- Linear4_test_error$ACF1

  linear4_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 4 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear4_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Linear4_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Linear 4 Autocorrelation function')

  linear4_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Linear4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Linear 4 model histogram of residuals")

  linear4_predictions <- time_series_test %>%
    fabletools::model(
      Linear4_model,
    ) %>%
    fabletools::forecast(h = number)

  Linear4_plot_of_forecast <- time_series_data %>%
    fabletools::model(Linear4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Linear4 forecast, \nNumber of time intervals = ", number))

  linear4_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Linear4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 4 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  linear4_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Linear4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Linear 4 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  linear4_prediction_model <- linear4_predictions[1]
  linear4_prediction_date<- linear4_predictions[2]
  linear4_prediction_range <- linear4_predictions[3]
  linear4_prediction_mean <-linear4_predictions[4]


  #<----- Mean -------------------->

  print("Working on the Mean model")

  Mean_start <- Sys.time()
  Mean_model <- fable::MEAN(Value)
  Mean_end <- Sys.time()
  Mean_duration <- Mean_end - Mean_start

  Mean_test_error <- time_series_train %>%
    fabletools::model(Mean_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Mean_RMSE <- Mean_test_error$RMSE
  Mean_Mean_Error <- Mean_test_error$ME
  Mean_Mean_Absolute_Error <- Mean_test_error$MAE
  Mean_Mean_Percentage_Error <- Mean_test_error$MPE

  Mean_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Mean_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Mean model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Mean_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Mean_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Mean Autocorrelation function')

  Mean_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Mean_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Mean model histogram of residuals")

  Mean_predictions <- time_series_test %>%
    fabletools::model(
      Mean_model,
    ) %>%
    fabletools::forecast(h = number)

  Mean_plot_of_forecast <- time_series_data %>%
    fabletools::model(Mean_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Mean forecast, \nNumber of time intervals = ", number))

  Mean_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Mean_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Mean model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Mean_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Mean_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Mean model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Mean_prediction_model <- Mean_predictions[1]
  Mean_prediction_date <- Mean_predictions[2]
  Mean_prediction_range <- Mean_predictions[3]
  Mean_prediction_mean <-Mean_predictions[4]

  #<----- Naive -------------------->

  print("Working on the Naive model")

  Naive_start <- Sys.time()
  Naive_model <- fable::NAIVE(Value)
  Naive_end <- Sys.time()
  Naive_duration <- Naive_end - Naive_start

  Naive_test_error <- time_series_train %>%
    fabletools::model(Naive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Naive_RMSE <- Naive_test_error$RMSE
  Naive_Mean_Error <- Naive_test_error$ME
  Naive_Mean_Absolute_Error <- Naive_test_error$MAE
  Naive_Mean_Percentage_Error <- Naive_test_error$MPE

  Naive_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Naive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Naive model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Naive_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Naive_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Naive Autocorrelation function')

  Naive_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Naive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Naive model histogram of residuals")

  Naive_predictions <- time_series_test %>%
    fabletools::model(
      Naive_model,
    ) %>%
    fabletools::forecast(h = number)

  Naive_plot_of_forecast <- time_series_data %>%
    fabletools::model(Naive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Naive forecast, \nNumber of time intervals = ", number))

  Naive_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Naive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Naive model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Naive_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Naive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Naive model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Naive_prediction_model <- Naive_predictions[1]
  Naive_prediction_date <- Naive_predictions[2]
  Naive_prediction_range <- Naive_predictions[3]
  Naive_prediction_mean <-Naive_predictions[4]

  #<----- Neuralnet 1 -------------------->

  print("Working on the Neuralnet models")

  Neuralnet1_start <- Sys.time()
  Neuralnet1_model <- fable::NNETAR(Value ~ season() + trend())
  Neuralnet1_end <- Sys.time()
  Neuralnet1_duration <- Neuralnet1_end - Neuralnet1_start

  Neuralnet1_test_error <- time_series_train %>%
    fabletools::model(Neuralnet1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Neuralnet1_RMSE <- Neuralnet1_test_error$RMSE
  Neuralnet1_Mean_Error <- Neuralnet1_test_error$ME
  Neuralnet1_Mean_Absolute_Error <- Neuralnet1_test_error$MAE
  Neuralnet1_Mean_Percentage_Error <- Neuralnet1_test_error$MPE

  Neuralnet1_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet1 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet1_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet1_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Neuralnet1 Autocorrelation function')

  Neuralnet1_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet1 model histogram of residuals")

  Neuralnet1_predictions <- time_series_test %>%
    fabletools::model(
      Neuralnet1_model,
    ) %>%
    fabletools::forecast(h = number)

  Neuralnet1_plot_of_forecast <- time_series_data %>%
    fabletools::model(Neuralnet1_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Neuralnet1 forecast, \nNumber of time intervals = ", number))

  Neuralnet1_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet1 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet1_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet1_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet1 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Neuralnet1_prediction_model <- Neuralnet1_predictions[1]
  Neuralnet1_prediction_date <- Neuralnet1_predictions[2]
  Neuralnet1_prediction_range <- Neuralnet1_predictions[3]
  Neuralnet1_prediction_mean <-Neuralnet1_predictions[4]


  #<----- Neuralnet 2 -------------------->

  Neuralnet2_start <- Sys.time()
  Neuralnet2_model <- fable::NNETAR(Value ~ trend())
  Neuralnet2_end <- Sys.time()
  Neuralnet2_duration <- Neuralnet2_end - Neuralnet2_start

  Neuralnet2_test_error <- time_series_train %>%
    fabletools::model(Neuralnet2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Neuralnet2_RMSE <- Neuralnet2_test_error$RMSE
  Neuralnet2_Mean_Error <- Neuralnet2_test_error$ME
  Neuralnet2_Mean_Absolute_Error <- Neuralnet2_test_error$MAE
  Neuralnet2_Mean_Percentage_Error <- Neuralnet2_test_error$MPE

  Neuralnet2_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet2 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet2_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet2_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Neuralnet2 Autocorrelation function')

  Neuralnet2_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet2 model histogram of residuals")

  Neuralnet2_predictions <- time_series_test %>%
    fabletools::model(
      Neuralnet2_model,
    ) %>%
    fabletools::forecast(h = number)

  Neuralnet2_plot_of_forecast <- time_series_data %>%
    fabletools::model(Neuralnet2_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Neuralnet2 forecast, \nNumber of time intervals = ", number))

  Neuralnet2_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet2 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet2_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet2_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet2 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Neuralnet2_prediction_model <- Neuralnet2_predictions[1]
  Neuralnet2_prediction_date <- Neuralnet2_predictions[2]
  Neuralnet2_prediction_range <- Neuralnet2_predictions[3]
  Neuralnet2_prediction_mean <-Neuralnet2_predictions[4]


  #<----- Neuralnet 3 -------------------->

  Neuralnet3_start <- Sys.time()
  Neuralnet3_model <- fable::NNETAR(Value ~ season())
  Neuralnet3_end <- Sys.time()
  Neuralnet3_duration <- Neuralnet3_end - Neuralnet3_start

  Neuralnet3_test_error <- time_series_train %>%
    fabletools::model(Neuralnet3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Neuralnet3_RMSE <- Neuralnet3_test_error$RMSE
  Neuralnet3_Mean_Error <- Neuralnet3_test_error$ME
  Neuralnet3_Mean_Absolute_Error <- Neuralnet3_test_error$MAE
  Neuralnet3_Mean_Percentage_Error <- Neuralnet3_test_error$MPE

  Neuralnet3_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet3 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet3_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet3_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Neuralnet3 Autocorrelation function')

  Neuralnet3_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet3 model histogram of residuals")

  Neuralnet3_predictions <- time_series_test %>%
    fabletools::model(
      Neuralnet3_model,
    ) %>%
    fabletools::forecast(h = number)

  Neuralnet3_plot_of_forecast <- time_series_data %>%
    fabletools::model(Neuralnet3_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Neuralnet3 forecast, \nNumber of time intervals = ", number))

  Neuralnet3_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet3 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet3_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet3_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet3 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Neuralnet3_prediction_model <- Neuralnet3_predictions[1]
  Neuralnet3_prediction_date <- Neuralnet3_predictions[2]
  Neuralnet3_prediction_range <- Neuralnet3_predictions[3]
  Neuralnet3_prediction_mean <-Neuralnet3_predictions[4]


  #<----- Neuralnet 4 -------------------->

  Neuralnet4_start <- Sys.time()
  Neuralnet4_model <- fable::NNETAR(Value)
  Neuralnet4_end <- Sys.time()
  Neuralnet4_duration <- Neuralnet4_end - Neuralnet4_start

  Neuralnet4_test_error <- time_series_train %>%
    fabletools::model(Neuralnet4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Neuralnet4_RMSE <- Neuralnet4_test_error$RMSE
  Neuralnet4_Mean_Error <- Neuralnet4_test_error$ME
  Neuralnet4_Mean_Absolute_Error <- Neuralnet4_test_error$MAE
  Neuralnet4_Mean_Percentage_Error <- Neuralnet4_test_error$MPE

  Neuralnet4_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet4 model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet4_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet4_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Neuralnet4 Autocorrelation function')

  Neuralnet4_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Neuralnet4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Neuralnet4 model histogram of residuals")

  Neuralnet4_predictions <- time_series_test %>%
    fabletools::model(
      Neuralnet4_model,
    ) %>%
    fabletools::forecast(h = number)

  Neuralnet4_plot_of_forecast <- time_series_data %>%
    fabletools::model(Neuralnet4_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Neuralnet4 forecast, \nNumber of time intervals = ", number))

  Neuralnet4_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet4 model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Neuralnet4_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Neuralnet4_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Neuralnet4 model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Neuralnet4_prediction_model <- Neuralnet4_predictions[1]
  Neuralnet4_prediction_date <- Neuralnet4_predictions[2]
  Neuralnet4_prediction_range <- Neuralnet4_predictions[3]
  Neuralnet4_prediction_mean <-Neuralnet4_predictions[4]


  #<----- Prophet Additive -------------------->

  print("Working on the Prophet models")

  Prophet_Additive_start <- Sys.time()
  Prophet_Additive_model <- fable.prophet::prophet(Value ~ season(period = 12, type = "additive"))
  Prophet_Additive_end <- Sys.time()
  Prophet_Additive_duration <- Prophet_Additive_end - Prophet_Additive_start

  Prophet_Additive_test_error <- time_series_train %>%
    fabletools::model(Prophet_Additive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Prophet_Additive_RMSE <- Prophet_Additive_test_error$RMSE
  Prophet_Additive_Mean_Error <- Prophet_Additive_test_error$ME
  Prophet_Additive_Mean_Absolute_Error <- Prophet_Additive_test_error$MAE
  Prophet_Additive_Mean_Percentage_Error <- Prophet_Additive_test_error$MPE

  Prophet_Additive_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Prophet_Additive model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Prophet_Additive_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Additive_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Prophet_Additive Autocorrelation function')

  Prophet_Additive_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Prophet_Additive model histogram of residuals")

  Prophet_Additive_predictions <- time_series_test %>%
    fabletools::model(
      Prophet_Additive_model,
    ) %>%
    fabletools::forecast(h = number)

  Prophet_Additive_plot_of_forecast <- time_series_data %>%
    fabletools::model(Prophet_Additive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Prophet_Additive forecast, \nNumber of time intervals = ", number))

  Prophet_Additive_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Prophet_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Prophet_Additive model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Prophet_Additive_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Prophet_Additive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Prophet_Additive model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Prophet_Additive_prediction_model <- Prophet_Additive_predictions[1]
  Prophet_Additive_prediction_date <- Prophet_Additive_predictions[2]
  Prophet_Additive_prediction_range <- Prophet_Additive_predictions[3]
  Prophet_Additive_prediction_mean <-Prophet_Additive_predictions[4]

  #<----- Prophet Multiplicative -------------------->

  Prophet_Multiplicative_start <- Sys.time()
  Prophet_Multiplicative_model <- fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative"))
  Prophet_Multiplicative_end <- Sys.time()
  Prophet_Multiplicative_duration <- Prophet_Multiplicative_end - Prophet_Multiplicative_start

  Prophet_Multiplicative_test_error <- time_series_train %>%
    fabletools::model(Prophet_Multiplicative_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  Prophet_Multiplicative_RMSE <- Prophet_Multiplicative_test_error$RMSE
  Prophet_Multiplicative_Mean_Error <- Prophet_Multiplicative_test_error$ME
  Prophet_Multiplicative_Mean_Absolute_Error <- Prophet_Multiplicative_test_error$MAE
  Prophet_Multiplicative_Mean_Percentage_Error <- Prophet_Multiplicative_test_error$MPE

  Prophet_Multiplicative_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Prophet_Multiplicative model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Prophet_Multiplicative_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Multiplicative_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Prophet_Multiplicative Autocorrelation function')

  Prophet_Multiplicative_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Prophet_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Prophet_Multiplicative model histogram of residuals")

  Prophet_Multiplicative_predictions <- time_series_test %>%
    fabletools::model(
      Prophet_Multiplicative_model,
    ) %>%
    fabletools::forecast(h = number)

  Prophet_Multiplicative_plot_of_forecast <- time_series_data %>%
    fabletools::model(Prophet_Multiplicative_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Prophet_Multiplicative forecast, \nNumber of time intervals = ", number))

  Prophet_Multiplicative_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Prophet_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Prophet_Multiplicative model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Prophet_Multiplicative_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Prophet_Multiplicative_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Prophet_Multiplicative model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Prophet_Multiplicative_prediction_model <- Prophet_Multiplicative_predictions[1]
  Prophet_Multiplicative_prediction_date <- Prophet_Multiplicative_predictions[2]
  Prophet_Multiplicative_prediction_range <- Prophet_Multiplicative_predictions[3]
  Prophet_Multiplicative_prediction_mean <-Prophet_Multiplicative_predictions[4]


  #<----- SNaive -------------------->

  print("Working on the Seasonal Naive model")

  SNaive_start <- Sys.time()
  SNaive_model <- fable::SNAIVE(Value)
  SNaive_end <- Sys.time()
  SNaive_duration <- SNaive_end - SNaive_start

  SNaive_test_error <- time_series_train %>%
    fabletools::model(SNaive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test) %>%
    dplyr::select(.model:MPE)

  SNaive_RMSE <- SNaive_test_error$RMSE
  SNaive_Mean_Error <- SNaive_test_error$ME
  SNaive_Mean_Absolute_Error <- SNaive_test_error$MAE
  SNaive_Mean_Percentage_Error <- SNaive_test_error$MPE

  SNaive_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(SNaive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("SNaive model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  SNaive_ACF <- fabletools::augment(time_series_data %>% fabletools::model(SNaive_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'SNaive Autocorrelation function')

  SNaive_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(SNaive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("SNaive model histogram of residuals")

  SNaive_predictions <- time_series_test %>%
    fabletools::model(
      SNaive_model,
    ) %>%
    fabletools::forecast(h = number)

  SNaive_plot_of_forecast <- time_series_data %>%
    fabletools::model(SNaive_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("SNaive forecast, \nNumber of time intervals = ", number))

  SNaive_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(SNaive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("SNaive model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  SNaive_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(SNaive_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("SNaive model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  SNaive_prediction_model <- SNaive_predictions[1]
  SNaive_prediction_date <- SNaive_predictions[2]
  SNaive_prediction_range <- SNaive_predictions[3]
  SNaive_prediction_mean <-SNaive_predictions[4]


  #<----- Stochastic -------------------->

  print("Working on the Stochastic model")

  Stochastic_start <- Sys.time()
  Stochastic_model <- fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
  Stochastic_end <- Sys.time()
  Stochastic_duration <- Stochastic_end - Stochastic_start

  Stochastic_test_error <- time_series_train %>%
    fabletools::model(Stochastic_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::accuracy(time_series_test)

  Stochastic_RMSE <- Stochastic_test_error$RMSE
  Stochastic_Mean_Error <- Stochastic_test_error$ME
  Stochastic_Mean_Absolute_Error <- Stochastic_test_error$MAE
  Stochastic_Mean_Percentage_Error <- Stochastic_test_error$MPE

  Stochastic_innovation_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Stochastic = Stochastic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Date, y = .innov)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ggtitle("Stochastic model innovation residuals") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Stochastic_ACF <- fabletools::augment(time_series_data %>% fabletools::model(Stochastic = Stochastic_model)) %>% drop_na() %>%
    feasts::ACF(.resid) %>%
    fabletools::autoplot() +
    ggplot2::labs(title = 'Stochastic Autocorrelation function')

  Stochastic_histogram_of_residuals <- hist_residuals <- fabletools::augment(time_series_data %>% fabletools::model(Stochastic = Stochastic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = .resid)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = 0, color = "red") +
    ggplot2::ggtitle("Stochastic model histogram of residuals")

  Stochastic_predictions <- time_series_test %>%
    fabletools::model(
      Stochastic = Stochastic_model,
    ) %>%
    fabletools::forecast(h = number)

  Stochastic_plot_of_forecast <- time_series_data %>%
    fabletools::model(Stochastic_model) %>%
    fabletools::forecast(h = number) %>%
    fabletools::autoplot(time_series_data) +
    ggplot2::labs(title = paste0("Stochastic forecast, \nNumber of time intervals = ", number))

  Stochastic_predicted_vs_actual <- fabletools::augment(time_series_train %>% fabletools::model(Stochastic = Stochastic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .fitted)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Stochastic model test data, \nPredicted vs actual") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth =1)

  Stochastic_predicted_vs_residual <- fabletools::augment(time_series_train %>% fabletools::model(Stochastic = Stochastic_model)) %>% drop_na() %>%
    ggplot2::ggplot(aes(x = Value, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Stochastic model test data, \nPredicted vs residual") +
    ggplot2::geom_hline(yintercept = 0, color = "red")

  Stochastic_prediction_model <- Stochastic_predictions[1]
  Stochastic_prediction_date<- Stochastic_predictions[2]
  Stochastic_prediction_range <- Stochastic_predictions[3]
  Stochastic_prediction_mean <-Stochastic_predictions[4]


  #<----- Numerical Results -------------------->

  Error_Results <-
    data.frame(
      'Prediction_Model' = c("Linear 1 model", "Linear 2 model", "Linear 3 model", "Linear 4 model",
         "ARIMA 1 model", "ARIMA 2 model", "ARIMA 3 model", "ARIMA 4 model", "Deterministic model", "Stochastic model",
         "ETS1", "ETS2", "ETS3", "ETS4",
         "Holt_Winters_Additive", "Holt_Winters_Multiplicative", "Holt_Winters_Damped",
         "Mean", "Naive", "Seasona_Naive", "Drift",
         "Prophet_Multiplicative", "Prophet_Additive",
         "Neuralnet1", "Neuralnet2", "Neuralnet3", "Neuralnet4", "Ensemble"),
      'RMSE' = round(c(Linear1_RMSE, Linear2_RMSE, Linear3_RMSE, Linear4_RMSE,
         Arima1_RMSE, Arima2_RMSE, Arima3_RMSE, Arima4_RMSE, Deterministic_RMSE, Stochastic_RMSE,
         Ets1_RMSE, Ets2_RMSE, Ets3_RMSE, Ets4_RMSE,
         Holt_Winters_Additive_RMSE, Holt_Winters_Multiplicative_RMSE, Holt_Winters_Damped_RMSE,
         Mean_RMSE, Naive_RMSE, SNaive_RMSE, Drift_RMSE,
         Prophet_Multiplicative_RMSE, Prophet_Additive_RMSE,
         Neuralnet1_RMSE, Neuralnet2_RMSE, Neuralnet3_RMSE, Neuralnet4_RMSE, Ensembles_RMSE), 4),
      'Mean_Error' = round(c(Linear1_Mean_Error, Linear2_Mean_Error, Linear1_Mean_Error, Linear4_Mean_Error,
         Arima1_Mean_Error, Arima2_Mean_Error, Arima3_Mean_Error, Arima4_Mean_Error, Deterministic_Mean_Error, Stochastic_Mean_Error,
         Ets1_Mean_Error, Ets2_Mean_Error, Ets3_Mean_Error, Ets4_Mean_Error,
         Holt_Winters_Additive_Mean_Error, Holt_Winters_Multiplicative_Mean_Error, Holt_Winters_Damped_Mean_Error,
         Mean_Mean_Error, Naive_Mean_Error, SNaive_Mean_Error, Drift_Mean_Error,
         Prophet_Multiplicative_Mean_Error, Prophet_Additive_Mean_Error,
         Neuralnet1_Mean_Error, Neuralnet2_Mean_Error, Neuralnet3_Mean_Error, Neuralnet4_Mean_Error, Ensembles_Mean_Error), 4),
      'Mean_Absolute_Error' = round(c(Linear1_Mean_Absolute_Error, Linear2_Mean_Absolute_Error, Linear3_Mean_Absolute_Error, Linear4_Mean_Absolute_Error,
         Arima1_Mean_Absolute_Error, Arima2_Mean_Absolute_Error, Arima3_Mean_Absolute_Error, Arima4_Mean_Absolute_Error, Deterministic_Mean_Absolute_Error, Stochastic_Mean_Absolute_Error,
         Ets1_Mean_Absolute_Error, Ets2_Mean_Absolute_Error, Ets3_Mean_Absolute_Error, Ets4_Mean_Absolute_Error,
         Holt_Winters_Additive_Mean_Absolute_Error, Holt_Winters_Multiplicative_Mean_Absolute_Error, Holt_Winters_Damped_Mean_Absolute_Error,
         Mean_Mean_Absolute_Error, Naive_Mean_Absolute_Error, SNaive_Mean_Absolute_Error, Drift_Mean_Absolute_Error,
         Prophet_Multiplicative_Mean_Absolute_Error, Prophet_Additive_Mean_Absolute_Error,
         Neuralnet1_Mean_Absolute_Error, Neuralnet2_Mean_Absolute_Error, Neuralnet3_Mean_Absolute_Error, Neuralnet4_Mean_Absolute_Error, Ensembles_Mean_Absolute_Error), 4),
      'Mean_Percentage_Error' = round(c(Linear1_Mean_Percentage_Error, Linear2_Mean_Percentage_Error,Linear3_Mean_Percentage_Error, Linear4_Mean_Percentage_Error,
         Arima1_Mean_Percentage_Error, Arima2_Mean_Percentage_Error, Arima3_Mean_Percentage_Error, Arima4_Mean_Percentage_Error, Deterministic_Mean_Percentage_Error, Stochastic_Mean_Percentage_Error,
         Ets1_Mean_Percentage_Error, Ets2_Mean_Percentage_Error, Ets3_Mean_Percentage_Error, Ets4_Mean_Percentage_Error,
         Holt_Winters_Additive_Mean_Percentage_Error, Holt_Winters_Multiplicative_Mean_Percentage_Error, Holt_Winters_Damped_Mean_Percentage_Error,
         Mean_Mean_Percentage_Error, Naive_Mean_Percentage_Error, SNaive_Mean_Percentage_Error, Drift_Mean_Percentage_Error,
         Prophet_Multiplicative_Mean_Percentage_Error, Prophet_Additive_Mean_Percentage_Error,
         Neuralnet1_Mean_Percentage_Error, Neuralnet2_Mean_Percentage_Error, Neuralnet3_Mean_Percentage_Error, Neuralnet4_Mean_Percentage_Error, Ensembles_Mean_Percentage_Error), 4),

      'Duration' = round(c(Linear1_duration, Linear2_duration, Linear3_duration, Linear4_duration,
         Arima1_duration, Arima2_duration, Arima3_duration, Arima4_duration, Deterministic_duration, Stochastic_duration,
         Ets1_duration, Ets2_duration, Ets3_duration, Ets4_duration,
         Holt_Winters_Additive_duration, Holt_Winters_Multiplicative_duration, Holt_Winters_Damped_duration,
         Mean_duration, Naive_duration, SNaive_duration, Drift_duration,
         Prophet_Multiplicative_duration, Prophet_Additive_duration,
         Neuralnet1_duration, Neuralnet2_duration, Neuralnet3_duration, Neuralnet4_duration, Ensembles_duration), 4)
    )

  Error_Results <- Error_Results %>%
    dplyr::arrange(RMSE)

  Error_Results_table <- reactable::reactable(Error_Results,
                                              searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                              striped = TRUE, highlight = TRUE, resizable = TRUE
  ) %>%
    reactablefmtr::add_title("Time series error results, click each column name to sort.")

  Table_of_predictions <- list(
    'Arima1' = Arima1_predictions, 'Arima2' = Arima2_predictions, 'Arima3' = Arima3_predictions, 'Arima4' = Arima4_predictions, 'Deterministic' = Deterministic_predictions,
    'Drift' = Drift_predictions, 'Ensembles' = Ensembles_predictions, 'Ets1' = Ets1_predictions, 'Ets2' = Ets2_predictions,
    'Ets3' = Ets3_predictions, 'Ets4' = Ets4_predictions, 'Holt_Winters_Additive' = Holt_Winters_Additive_predictions,
    'Holt_Winters_Multiplicative' = Holt_Winters_Multiplicative_predictions, 'Holt_Winters_Damped' = Holt_Winters_Damped_predictions,
    'Linear1' = linear1_predictions, 'Linear2' = linear2_predictions, 'Linear3' = linear3_predictions, 'Linear4' = linear4_predictions,
    'Mean' = Mean_predictions, 'Naive' = Naive_predictions,
    'Neuralnet1' = Neuralnet1_predictions, 'Neuralnet2' = Neuralnet2_predictions, 'Neuralnet3' = Neuralnet3_predictions, 'Neuralnet4' = Neuralnet4_predictions,
    'Prophet_Additive' = Prophet_Additive_predictions, 'Prophet_Multiplicative' = Prophet_Multiplicative_predictions,
    'SNaive' = SNaive_predictions, 'Stochastic' = Stochastic_predictions)

  Table_of_predictions

  best_results_rmse <- gt::gt(as.data.frame(Table_of_predictions[Error_Results[order(Error_Results$RMSE), ][1, 1]])) %>%
    gt::tab_header(
      title = "Best results RMSE"
    )


  best_results_mae <- gt::gt(as.data.frame(Table_of_predictions[Error_Results[order(Error_Results$Mean_Absolute_Error), ][1, 1]])) %>%
    gt::tab_header(
      title = "Best Results Mean AbsoluteError"
    )

  best_results_mean_error <- gt::gt(as.data.frame(Table_of_predictions[Error_Results[which.min(abs(Error_Results$Mean_Error - 0)), ][1, 1]])) %>%
    gt::tab_header(
      title = "Best Results Mean Error"
    )


  best_results_mpe <- gt::gt(as.data.frame(Table_of_predictions[Error_Results[which.min(abs(Error_Results$Mean_Percentage_Error - 0)), ][1, 1]])) %>%
    gt::tab_header(
      title = "Best Results Mean Percentage Error"
    )


  #<----- Graphical Results by Model -------------------->
  gridExtra::grid.arrange(Arima1_predicted_vs_actual, Arima1_predicted_vs_residual, Arima1_histogram_of_residuals, Arima1_innovation_residuals, Arima1_ACF, Arima1_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Arima2_predicted_vs_actual, Arima2_predicted_vs_residual, Arima2_histogram_of_residuals, Arima2_innovation_residuals, Arima2_ACF, Arima2_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Arima3_predicted_vs_actual, Arima3_predicted_vs_residual, Arima3_histogram_of_residuals, Arima3_innovation_residuals, Arima3_ACF, Arima3_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Arima4_predicted_vs_actual, Arima4_predicted_vs_residual, Arima4_histogram_of_residuals, Arima4_innovation_residuals, Arima4_ACF, Arima4_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Deterministic_predicted_vs_actual, Deterministic_predicted_vs_residual, Deterministic_histogram_of_residuals, Deterministic_innovation_residuals, Deterministic_ACF, Deterministic_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Drift_predicted_vs_actual, Drift_predicted_vs_residual, Drift_histogram_of_residuals, Drift_innovation_residuals, Drift_ACF, Drift_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Ensemble_predicted_vs_actual, Ensemble_predicted_vs_residual, Ensemble_histogram_of_residuals, Ensemble_innovation_residuals, Ensemble_ACF, Ensemble_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Ets1_predicted_vs_actual, Ets1_predicted_vs_residual, Ets1_histogram_of_residuals, Ets1_innovation_residuals, Ets1_ACF, Ets1_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Ets2_predicted_vs_actual, Ets2_predicted_vs_residual, Ets2_histogram_of_residuals, Ets2_innovation_residuals, Ets2_ACF, Ets2_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Ets3_predicted_vs_actual, Ets3_predicted_vs_residual, Ets3_histogram_of_residuals, Ets3_innovation_residuals, Ets3_ACF, Ets3_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Ets4_predicted_vs_actual, Ets4_predicted_vs_residual, Ets4_histogram_of_residuals, Ets4_innovation_residuals, Ets4_ACF, Ets4_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Holt_Winters_Additive_predicted_vs_actual, Holt_Winters_Additive_predicted_vs_residual, Holt_Winters_Additive_histogram_of_residuals, Holt_Winters_Additive_innovation_residuals, Holt_Winters_Additive_ACF, Holt_Winters_Additive_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Holt_Winters_Multiplicative_predicted_vs_actual, Holt_Winters_Multiplicative_predicted_vs_residual, Holt_Winters_Multiplicative_histogram_of_residuals, Holt_Winters_Multiplicative_innovation_residuals, Holt_Winters_Multiplicative_ACF, Holt_Winters_Multiplicative_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Holt_Winters_Damped_predicted_vs_actual, Holt_Winters_Damped_predicted_vs_residual, Holt_Winters_Damped_histogram_of_residuals, Holt_Winters_Damped_innovation_residuals, Holt_Winters_Damped_ACF, Holt_Winters_Damped_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(linear1_predicted_vs_actual, linear1_predicted_vs_residual, linear1_histogram_of_residuals, linear1_innovation_residuals, linear1_ACF, Linear1_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(linear2_predicted_vs_actual, linear2_predicted_vs_residual, linear2_histogram_of_residuals, linear2_innovation_residuals, linear2_ACF, Linear2_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(linear3_predicted_vs_actual, linear3_predicted_vs_residual, linear3_histogram_of_residuals, linear3_innovation_residuals, linear3_ACF, Linear3_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(linear4_predicted_vs_actual, linear4_predicted_vs_residual, linear4_histogram_of_residuals, linear4_innovation_residuals, linear4_ACF, Linear4_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Mean_predicted_vs_actual, Mean_predicted_vs_residual, Mean_histogram_of_residuals, Mean_innovation_residuals , Mean_ACF, Mean_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Naive_predicted_vs_actual, Naive_predicted_vs_residual, Naive_histogram_of_residuals, Naive_innovation_residuals , Naive_ACF, Naive_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Neuralnet1_predicted_vs_actual, Neuralnet1_predicted_vs_residual, Neuralnet1_histogram_of_residuals, Neuralnet1_innovation_residuals , Neuralnet1_ACF, Neuralnet1_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Neuralnet2_predicted_vs_actual, Neuralnet2_predicted_vs_residual, Neuralnet2_histogram_of_residuals, Neuralnet2_innovation_residuals , Neuralnet2_ACF, Neuralnet2_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Neuralnet3_predicted_vs_actual, Neuralnet3_predicted_vs_residual, Neuralnet3_histogram_of_residuals, Neuralnet3_innovation_residuals , Neuralnet3_ACF, Neuralnet3_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Neuralnet4_predicted_vs_actual, Neuralnet4_predicted_vs_residual, Neuralnet4_histogram_of_residuals, Neuralnet4_innovation_residuals , Neuralnet4_ACF, Neuralnet4_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Prophet_Additive_predicted_vs_actual, Prophet_Additive_predicted_vs_residual, Prophet_Additive_histogram_of_residuals, Prophet_Additive_innovation_residuals , Prophet_Additive_ACF, Prophet_Additive_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Prophet_Multiplicative_predicted_vs_actual, Prophet_Multiplicative_predicted_vs_residual, Prophet_Multiplicative_histogram_of_residuals, Prophet_Multiplicative_innovation_residuals , Prophet_Multiplicative_ACF, Prophet_Multiplicative_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(SNaive_predicted_vs_actual, SNaive_predicted_vs_residual, SNaive_histogram_of_residuals, SNaive_innovation_residuals, SNaive_ACF, SNaive_plot_of_forecast, ncol = 2)
  gridExtra::grid.arrange(Stochastic_predicted_vs_actual, Stochastic_predicted_vs_residual, Stochastic_histogram_of_residuals, Stochastic_innovation_residuals , Stochastic_ACF, Stochastic_plot_of_forecast, ncol = 2)

  # <-------------- Graphical Results by Measure # -------------->

  gridExtra::grid.arrange(Arima1_predicted_vs_actual, Arima2_predicted_vs_actual, Arima3_predicted_vs_actual, Arima4_predicted_vs_actual, Deterministic_predicted_vs_actual, Drift_predicted_vs_actual,
    Ensemble_predicted_vs_actual, Ets1_predicted_vs_actual, Ets2_predicted_vs_actual, Ets3_predicted_vs_actual, Ets4_predicted_vs_actual,
    Holt_Winters_Additive_predicted_vs_actual, Holt_Winters_Multiplicative_predicted_vs_actual, Holt_Winters_Damped_predicted_vs_actual,
    linear1_predicted_vs_actual, linear2_predicted_vs_actual, linear3_predicted_vs_actual, linear4_predicted_vs_actual,
    Mean_predicted_vs_actual, Naive_predicted_vs_actual,
    Neuralnet1_predicted_vs_actual, Neuralnet2_predicted_vs_actual, Neuralnet3_predicted_vs_actual, Neuralnet4_predicted_vs_actual,
    Prophet_Additive_predicted_vs_actual, Prophet_Multiplicative_predicted_vs_actual, SNaive_predicted_vs_actual, Stochastic_predicted_vs_actual)

  gridExtra::grid.arrange(Arima1_predicted_vs_residual, Arima2_predicted_vs_residual, Arima3_predicted_vs_residual, Arima4_predicted_vs_residual, Deterministic_predicted_vs_residual, Drift_predicted_vs_residual,
    Ensemble_predicted_vs_residual,  Ets1_predicted_vs_residual, Ets2_predicted_vs_residual, Ets3_predicted_vs_residual, Ets4_predicted_vs_residual,
    Holt_Winters_Additive_predicted_vs_residual, Holt_Winters_Multiplicative_predicted_vs_residual, Holt_Winters_Damped_predicted_vs_residual,
    linear1_predicted_vs_residual, linear2_predicted_vs_residual, linear3_predicted_vs_residual, linear4_predicted_vs_residual,
    Mean_predicted_vs_residual, Naive_predicted_vs_residual,
    Neuralnet1_predicted_vs_residual, Neuralnet2_predicted_vs_residual, Neuralnet3_predicted_vs_residual, Neuralnet4_predicted_vs_residual,
    Prophet_Additive_predicted_vs_residual, Prophet_Multiplicative_predicted_vs_residual, SNaive_predicted_vs_residual, Stochastic_predicted_vs_residual)

  gridExtra::grid.arrange(Arima1_histogram_of_residuals, Arima2_histogram_of_residuals, Arima3_histogram_of_residuals, Arima4_histogram_of_residuals, Deterministic_histogram_of_residuals, Drift_histogram_of_residuals,
    Ensemble_histogram_of_residuals, Ets1_histogram_of_residuals, Ets2_histogram_of_residuals, Ets3_histogram_of_residuals, Ets4_histogram_of_residuals,
    Holt_Winters_Additive_histogram_of_residuals, Holt_Winters_Multiplicative_histogram_of_residuals, Holt_Winters_Damped_histogram_of_residuals,
    linear1_histogram_of_residuals, linear2_histogram_of_residuals, linear3_histogram_of_residuals, linear4_histogram_of_residuals,
    Mean_histogram_of_residuals, Naive_histogram_of_residuals,
    Neuralnet1_histogram_of_residuals, Neuralnet2_histogram_of_residuals, Neuralnet3_histogram_of_residuals, Neuralnet4_histogram_of_residuals,
    Prophet_Additive_histogram_of_residuals, Prophet_Multiplicative_histogram_of_residuals, SNaive_histogram_of_residuals, Stochastic_histogram_of_residuals)

  gridExtra::grid.arrange(Arima1_innovation_residuals, Arima2_innovation_residuals, Arima3_innovation_residuals, Arima4_innovation_residuals, Deterministic_innovation_residuals, Drift_innovation_residuals,
    Ensemble_innovation_residuals, Ets1_innovation_residuals, Ets2_innovation_residuals, Ets3_innovation_residuals, Ets4_innovation_residuals,
    Holt_Winters_Additive_innovation_residuals, Holt_Winters_Multiplicative_innovation_residuals, Holt_Winters_Damped_innovation_residuals,
    linear1_innovation_residuals, linear2_innovation_residuals, linear3_innovation_residuals, linear4_innovation_residuals,
    Mean_innovation_residuals, Naive_innovation_residuals,
    Neuralnet1_innovation_residuals, Neuralnet2_innovation_residuals, Neuralnet3_innovation_residuals, Neuralnet4_innovation_residuals,
    Prophet_Additive_innovation_residuals, Prophet_Multiplicative_innovation_residuals, SNaive_innovation_residuals, Stochastic_innovation_residuals)

  gridExtra::grid.arrange(Arima1_ACF, Arima2_ACF, Arima3_ACF, Arima4_ACF, Deterministic_ACF, Drift_ACF,
    Ensemble_ACF, Ets1_ACF, Ets2_ACF, Ets3_ACF, Ets4_ACF,
    Holt_Winters_Additive_ACF, Holt_Winters_Multiplicative_ACF, Holt_Winters_Damped_ACF,
    linear1_ACF, linear2_ACF, linear3_ACF, linear4_ACF,
    Mean_ACF, Naive_ACF,
    Neuralnet1_ACF, Neuralnet2_ACF, Neuralnet3_ACF, Neuralnet4_ACF,
    Prophet_Additive_ACF, Prophet_Multiplicative_ACF, SNaive_ACF, Stochastic_ACF)

  gridExtra::grid.arrange(Arima1_plot_of_forecast, Arima2_plot_of_forecast, Arima3_plot_of_forecast, Arima4_plot_of_forecast, Deterministic_plot_of_forecast, Drift_plot_of_forecast,
    Ensemble_plot_of_forecast, Ets1_plot_of_forecast, Ets2_plot_of_forecast, Ets3_plot_of_forecast, Ets4_plot_of_forecast,
    Holt_Winters_Additive_plot_of_forecast, Holt_Winters_Multiplicative_plot_of_forecast, Holt_Winters_Damped_plot_of_forecast,
    Linear1_plot_of_forecast, Linear2_plot_of_forecast, Linear3_plot_of_forecast, Linear4_plot_of_forecast,
    Mean_plot_of_forecast, Naive_plot_of_forecast,
    Neuralnet1_plot_of_forecast, Neuralnet2_plot_of_forecast, Neuralnet3_plot_of_forecast, Neuralnet4_plot_of_forecast,
    Prophet_Additive_plot_of_forecast, Prophet_Multiplicative_plot_of_forecast, SNaive_plot_of_forecast, Stochastic_plot_of_forecast)

  return(  list('All_Time_Series_Features' = all_time_series_features, 'Seasonal_plots' = seasonal_plots, 'Lag_plots' = lag_plots, 'Quartiles' = time_series_quartiles, 'Quintiles'= time_series_difference_quartiles, 'Baseline_Full' = baseline_full, 'Baseline_Difference' = baseline_difference, 'Head_of_Time_Series' = head_of_time_series, tail_of_time_series,
                'Basic_Graph' = basic_graph, 'Full_Time_Series' = full_time_series, 'Full_timr_Series_2' = full_time_series_2, 'Value_and_trend' = value_and_trend, 'Value_and_seasonally_adjusted' = value_and_seasonally_adjusted, 'Difference_Plot_1' = difference_plot1, 'Difference_plot_2' = difference_plot2,
                'Best_Results_RMSE' = best_results_rmse, 'Best_Results_Mean_Absolute_Error' = best_results_mae, 'Best_Results_Mean_Error' = best_results_mean_error,
                'Best_Results_Mean_Percentage_Error' = best_results_mpe,
                'Difference_plot_3' = difference_plot3, 'Difference_plot_4' = difference_plot4, 'Time_Series_Anamolies' = Print_Time_Series_Anomalies, 'Time_Series_Difference_Anomolies' = Print_Time_Series_Difference_Anomalies, 'GGPairs_1' = ggpairs1, 'GGPairs_2' = ggpairs2, 'Error_Results' = Error_Results_table, 'Table_of_predictions' = Table_of_predictions))
}
