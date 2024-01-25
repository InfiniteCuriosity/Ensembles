#' forecasting—function to perform time series analysis and return the results to the user.
#'
#' @param time_series_data a time series
#' @param number_of_intervals_to_forecast the number of intervals, such as months or weeks, that are going to be forecast
#' @param time_interval user states whether the time interval is quarterly, monthly or weekly.
#' @param use_parallel "Y" or "N" for parallel processing
#'
#' @returns A series of summary reports and visualizations to fully describe the time series: Forecast accuracy, forecast numbers, forecast plot, innovation residuals,
#' @returns best autocorrelation function (ACF), plot of best histogram of residuals, plot of best actual vs predicted, plot of best actual vs trend
#' @returns plot of best actual vs seasonally adjusted
#' @export forecasting
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

forecasting <- function(time_series_data, number_of_intervals_to_forecast, use_parallel = c("Y", "N"), time_interval = c("Q", "M", "W")) {
  use_parallel <- 0
  no_cores <- 0

  if (use_parallel == "Y") {
    cl <- parallel::makeCluster(no_cores, type = "FORK")
    doParallel::registerDoParallel(cl)
  }

  time_series_data <- time_series_data

  Label <- 0
  Value <- 0
  Month <- 0
  Change <- 0
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


  if (time_interval == "Q") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearquarter(Label), Value = Value, Change = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Change) %>%
      fabletools::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }
  if (time_interval == "M") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearmonth(Label), Value = Value, Change = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Change) %>%
      fabletools::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }
  if (time_interval == "W") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Date = tsibble::yearweek(Label), Value = Value, Change = tsibble::difference(Value)) %>%
      dplyr::select(Date, Value, Change) %>%
      fabletools::as_tsibble(index = Date) %>%
      dplyr::slice(-c(1))
  }


  # <----- Plot of Value ----------------------------------------------> ####
  plot_of_value <- time_series_data %>%
    ggplot2::ggplot(aes(x = Date, y = Value)) +
    ggplot2::geom_line(aes(x = Date, y = Value)) +
    ggplot2::geom_point(aes(x = Date, y = Value)) +
    ggplot2::labs(title = "Value per unit of time")


  # <----- Plot of individual seasons ----------------------------------------------> ####
  plot_of_individual_seasons <- time_series_data %>%
    feasts::gg_season(Value) +
    ggplot2::labs(title = "Plot of individual seasons")

  # <----- Plot of subseasons ----------------------------------------------> ####
  plot_of_subseasons <- time_series_data %>%
    feasts::gg_subseries(y = Value) +
    ggplot2::labs(title = "Plot of subseasons")

  # <----- Plot of multiple lags ---------------------------------------------->
  plot_of_multiple_lags <- time_series_data %>%
    feasts::gg_lag(y = Value, geom = "point") +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Plot of multiple lags")

  # <----- Tail of Table of Value ---------------------------------------------> ####

  table_of_value_tail <- gt::gt(tail(time_series_data[, c(1:2)]),
    caption = "Tail (most recent) of total value data set"
  ) %>%
    gt::fmt_number(columns = c("Value"), decimals = 0, use_seps = TRUE)

  # <----- Head of Table of Value ---------------------------------------------> ####

  table_of_value_head <- gt::gt(head(time_series_data[, c(1:2)]),
    caption = "Head (beginning) of total value data set"
  ) %>%
    gt::fmt_number(columns = c("Value"), decimals = 0, use_seps = TRUE)

  # <----- Plot of Trend of the Value ---------------------------------> ####
  time_series_decomposition <- time_series_data %>%
    fabletools::model(stl = STL(Value)) %>%
    fabletools::components()

  plot_of_trend <- time_series_decomposition %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(mapping = aes(x = Date, y = Value)) +
    ggplot2::geom_line(aes(x = Date, y = Value)) +
    ggplot2::geom_line(aes(x = Date, y = trend, color = "gray")) +
    ggplot2::labs(
      y = "Total number of value",
      title = "Total value in black, with trend (in red)"
    )

  # <----- Plot Current Value vs Seasonally Adjusted-> ####

  plot_of_seasonally_adjusted <- time_series_decomposition %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(mapping = aes(x = Date, y = season_adjust)) +
    ggplot2::geom_line(aes(x = Date, y = Value)) +
    ggplot2::geom_line(aes(x = Date, y = season_adjust, color = "gray")) +
    ggplot2::labs(
      y = "Total number of value",
      title = "Total value in black, with season_adjust (in red)"
    )

  # <----- Plot of Decomposition of the Value--------------------------> ####

  plot_of_decomposition <- time_series_decomposition %>%
    feasts::autoplot() +
    ggplot2::labs(title = "Plot of decomposition")

  # <----- Plot of Anomalies of the Value------------------------------> ####

  remainder <- time_series_decomposition$remainder

  remainder1 <- sd(remainder)

  time_series_anomalies <- ggplot2::ggplot(data = time_series_decomposition, aes(x = Date, y = remainder)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(remainder1, -remainder1), linetype = "dashed", color = "blue") +
    ggplot2::geom_hline(yintercept = c(2 * remainder1, -2 * remainder1), linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::labs(title = "Anomalies in value data \nblue line = 1 standard deviation +/- 0, red line = 2 standard deviations +/- 0")


  # <--- 34 individual models and seven ensembles to predict Value ----> ####

  time_series_train <- time_series_data[1:round(0.6 * nrow(time_series_data)), ] %>% tidyr::drop_na()
  time_series_test <- time_series_data[nrow(time_series_train) + 1:(nrow(time_series_data) - nrow(time_series_train)), ] %>% tidyr::drop_na()



  ##############################################

  ###### Start AICc methods here ###############

  ##############################################

  # These models will be evaluated using AICc
  Time_Series_AICc <- time_series_train %>%
    fabletools::model(
      Linear1 = TSLM(Value ~ season() + trend()),
      Linear2 = TSLM(Value),
      Linear3 = TSLM(Value ~ season()),
      Linear4 = TSLM(Value ~ trend()),
      Arima1 = ARIMA(Value ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima2 = ARIMA(Value ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima3 = ARIMA(Value ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima4 = ARIMA(Value),
      Deterministic = ARIMA(Value ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Stochastic = ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Ets1 = ETS(Value ~ season() + trend()),
      Ets2 = ETS(Value ~ trend()),
      Ets3 = ETS(Value ~ season()),
      Ets4 = ETS(Value),
      Holt_Winters_Additive = ETS(Value ~ error("A") + trend("A") + season("A")),
      Holt_Winters_Multiplicative = ETS(Value ~ error("M") + trend("A") + season("M")),
      Holt_Winters_Damped = ETS(Value ~ error("M") + trend("Ad") + season("M")),
      Fourier1 = ARIMA(log(Value) ~ fourier(K = 1) + PDQ(0, 0, 0)),
      Fourier2 = ARIMA(log(Value) ~ fourier(K = 2) + PDQ(0, 0, 0)),
      Fourier3 = ARIMA(log(Value) ~ fourier(K = 3) + PDQ(0, 0, 0)),
      Fourier4 = ARIMA(log(Value) ~ fourier(K = 4) + PDQ(0, 0, 0)),
      Fourier5 = ARIMA(log(Value) ~ fourier(K = 5) + PDQ(0, 0, 0)),
      Fourier6 = ARIMA(log(Value) ~ fourier(K = 6) + PDQ(0, 0, 0))
    )

  Time_Series_AICc_fit <- Time_Series_AICc %>%
    fabletools::glance(Time_Series_AIC_fit) %>%
    dplyr::select(.model, AIC, AICc, BIC) %>%
    dplyr::arrange(AICc)

  forecast_accuracy_AICc_table <- gt::gt(Time_Series_AICc_fit, caption = "Time series forecast accuracy, sorted by AICc")

  #### Evaluate results starting here ####

  if (Time_Series_AICc_fit[1, 1] == "Arima1") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Arima1 = fable::ARIMA(Value ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Arima 1 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 1 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 1 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 1 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima1 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima1 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Arima2") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Arima2 = ARIMA(Value ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Arima 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Arima 2 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 2 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 2 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 2 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 2 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 2 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Arima3") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Arima3 = ARIMA(Value ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Arima 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Arima 3 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 3 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 3 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 3 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 3 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 3 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Arima4") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Arima4 = ARIMA(Value)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Arima 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Arima 4 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 4 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 4 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 4 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 4 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 4 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Deterministic") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Deterministic = ARIMA(Value ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Deterministic") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Deterministic AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Deterministic AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Deterministic AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Deterministic histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Deterministic AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Deterministic AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Ets1") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Ets1 = ETS(Value ~ season() + trend())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Ets1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Ets1 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets1 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets1 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets1 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets1 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets1 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Ets2") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Ets2 = ETS(Value ~ trend())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Ets 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Ets 2 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 2 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 2 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 2 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 2 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 2 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Ets3") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Ets3 = ETS(Value ~ season())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Ets 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Ets 3 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 3 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 3 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 3 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 3 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 3 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Ets4") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Ets4 = ETS(Value)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Ets 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Ets 4 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 4 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 4 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 4 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 4 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 4 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier1") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier1 = ARIMA(log(Value) ~ fourier(K = 1) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 1 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 1 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 1 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 1 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 1 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 1 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier2") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier2 = ARIMA(log(Value) ~ fourier(K = 2) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 2 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 2 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 2 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 2 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 2 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 2 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier3") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier3 = ARIMA(log(Value) ~ fourier(K = 3) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 3 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 3 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 3 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 3 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 3 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 3 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier4") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier4 = ARIMA(log(Value) ~ fourier(K = 4) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 4 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 4 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 4 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 4 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 4 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 4 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier5") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier5 = ARIMA(log(Value) ~ fourier(K = 5) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 5") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 5 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 5 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 5 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 5 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 5 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 5 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Fourier6") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Fourier6 = ARIMA(log(Value) ~ fourier(K = 6) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Fourier 6") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Fourier 6 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 6 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 6 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 6 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 6 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 6 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Holt_Winters_Additive") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Additive = ETS(Value ~ error("A") + trend("A") + season("A"))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Holt-Winters Additive") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Holt-Winters Additive AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Additive AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Additive AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Additive histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Additive AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Additive AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Holt_Winters_Multiplicative") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Multiplicative = ETS(Value ~ error("M") + trend("A") + season("M"))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Holt-Winters Multiplicative") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Multiplicative histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Holt_Winters_Damped") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Damped = ETS(Value ~ error("M") + trend("Ad") + season("M"))
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Holt-Winters Damped") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Holt-Winters Damped AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Damped AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Damped AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Damped histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Damped AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Damped AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Linear1") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Linear1 = TSLM(Value ~ season() + trend())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Linear 1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Linear 1 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 1 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 1 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 1 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 1 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 1 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Linear2") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Linear2 = TSLM(Value)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Linear 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Linear 2 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 2 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 2 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 2 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 2 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 2 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Linear3") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Linear3 = TSLM(Value ~ season())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Linear 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Linear 3 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 3 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 3 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 3 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 3 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 3 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Linear4") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Linear4 = TSLM(Value ~ trend())
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Linear 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Linear 4 AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 4 AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 4 AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 4 histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 4 AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 4 AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_fit[1, 1] == "Stochastic") {
    Best_Model_AICc <- time_series_data %>%
      fabletools::model(
        Stochastic = ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc <- gt::gt(data = Best_Forecast_AICc, caption = "Best Forecast AICc using Stochastic") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc <- Best_Model_AICc %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = "Stochastic AICc model forecast of value") +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Stochastic AICc innovation residuals")

    Best_ACF_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Stochastic AICc Autocorrelation function")

    Best_Histogram_of_Residuals_AICc <- fabletools::augment(Best_Model_AICc) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Stochastic histogram of residuals")

    Best_Actual_vs_Predicted_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Stochastic AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Stochastic AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }





  #############################################################

  ####### Start AICc methods here to measure 1-unit change ####

  #############################################################

  Time_Series_AICc_Change <- time_series_train %>%
    fabletools::model(
      Linear1 = TSLM(Change ~ season() + trend()),
      Linear2 = TSLM(Change),
      Linear3 = TSLM(Change ~ season()),
      Linear4 = TSLM(Change ~ trend()),
      Arima1 = ARIMA(Change ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima2 = ARIMA(Change ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima3 = ARIMA(Change ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Arima4 = ARIMA(Change),
      Deterministic = ARIMA(Change ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Stochastic = ARIMA(Change ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
      Ets1 = ETS(Change ~ season() + trend()),
      Ets2 = ETS(Change ~ trend()),
      Ets3 = ETS(Change ~ season()),
      Ets4 = ETS(Change),
      Holt_Winters_Additive = ETS(Change ~ error("A") + trend("A") + season("A")),
      Holt_Winters_Multiplicative = ETS(Change ~ error("M") + trend("A") + season("M")),
      Holt_Winters_Damped = ETS(Change ~ error("M") + trend("Ad") + season("M")),
      Fourier1 = ARIMA(Change ~ fourier(K = 1) + PDQ(0, 0, 0)),
      Fourier2 = ARIMA(Change ~ fourier(K = 2) + PDQ(0, 0, 0)),
      Fourier3 = ARIMA(Change ~ fourier(K = 3) + PDQ(0, 0, 0)),
      Fourier4 = ARIMA(Change ~ fourier(K = 4) + PDQ(0, 0, 0)),
      Fourier5 = ARIMA(Change ~ fourier(K = 5) + PDQ(0, 0, 0)),
      Fourier6 = ARIMA(Change ~ fourier(K = 6) + PDQ(0, 0, 0))
    )

  Time_Series_AICc_Change_fit <- Time_Series_AICc_Change %>%
    fabletools::glance(Time_Series_AIC_fit) %>%
    dplyr::select(.model, AIC, AICc, BIC) %>%
    dplyr::arrange(AICc)

  forecast_accuracy_AICc_change_table <- gt::gt(Time_Series_AICc_Change_fit, caption = "Time Series Change Forecast Accuracy, sorted by AICc")

  #### Evaluate results starting here ####

  if (Time_Series_AICc_Change_fit[1, 1] == "Arima1") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Arima1 = fable::ARIMA(Change ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast for Change AICc") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 1 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 1 using AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 1 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 1 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima1 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima1 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Arima2") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Arima2 = ARIMA(Change ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Arima 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 2 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 2 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 2 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 2 AICc to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 2 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 2 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Arima3") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Arima3 = ARIMA(Change ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Arima 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 3 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 3 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 3 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 3 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 3 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 3 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Arima4") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Arima4 = ARIMA(Change)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Arima 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 4 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 4 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Arima 4 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Arima 4 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 4 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Arima 4 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Deterministic") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Deterministic = ARIMA(Change ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Deterministic") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Deterministic AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Deterministic AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Deterministic AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Deterministic AICc to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Deterministic AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Deterministic AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Ets1") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Ets1 = ETS(Change ~ season() + trend())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Ets1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets1 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets1 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets1 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets1 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets1 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets1 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Ets2") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Ets2 = ETS(Change ~ trend())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change,  using Ets 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 2 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 2 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 2 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 2 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 2 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 2 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Ets3") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Ets3 = ETS(Change ~ season())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Ets 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 3 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 3 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 3 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 3 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 3 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 3 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Ets4") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Ets4 = ETS(Change)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Ets 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 4 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 4 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Ets 4 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Ets 4 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 4 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Ets 4 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier1") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier1 = ARIMA(Change ~ fourier(K = 1) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 1 AICc to measure change,  model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 1 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 1 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 1 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 1 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 1 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier2") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier2 = ARIMA(Change ~ fourier(K = 2) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 2 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 2 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 2 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 2 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 2 to measure change, AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 2 AICc to measure change,  Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier3") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier3 = ARIMA(Change ~ fourier(K = 3) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 3 AICc model to measure change, forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 3 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 3 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 3 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 3 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 3 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier4") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier4 = ARIMA(Change ~ fourier(K = 4) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 4 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 4 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 4 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 4 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 4 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 4 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier5") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier5 = ARIMA(Change ~ fourier(K = 5) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 5") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 5 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 5 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 5 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 5 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 5 to measure change, AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 5 to measure change, AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Fourier6") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Fourier6 = ARIMA(Change ~ fourier(K = 6) + PDQ(0, 0, 0))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Fourier 6") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 6 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 6 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Fourier 6 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Fourier 6 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 6 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Fourier 6 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Holt_Winters_Additive") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Additive = ETS(Change ~ error("A") + trend("A") + season("A"))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Holt-Winters Additive") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Additive AICc to measure change, model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Additive AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Additive AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Additive to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Additive AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Additive AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Holt_Winters_Multiplicative") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Multiplicative = ETS(Change ~ error("M") + trend("A") + season("M"))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Holt-Winters Multiplicative") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Multiplicative histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Multiplicative AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Holt_Winters_Damped") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Damped = ETS(Change ~ error("M") + trend("Ad") + season("M"))
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Holt-Winters Damped") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Damped AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Damped AICc to measure change,  innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Holt-Winters Damped AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Holt-Winters Damped to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Damped to measure change, AICc Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Holt-Winters Damped to measure change, AICc Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Linear1") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Linear1 = TSLM(Change ~ season() + trend())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Linear 1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 1 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 1 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 1 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 1 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 1 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 1 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Linear2") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Linear2 = TSLM(Change)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Linear 2") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 2 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 2 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 2 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 2 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 2 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 2 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Linear3") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Linear3 = TSLM(Change ~ season())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Linear 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 3 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 3 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 3 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 3 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 3 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 3 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Linear4") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Linear4 = TSLM(Change ~ trend())
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Linear 4") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 4 AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 4 AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Linear 4 AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Linear 4 to measure change, histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 4 AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Linear 4 AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (Time_Series_AICc_Change_fit[1, 1] == "Stochastic") {
    Best_Model_AICc_Change <- time_series_data %>%
      fabletools::model(
        Stochastic = ARIMA(Change ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast_AICc_Change <- gt::gt(data = Best_Forecast_AICc_Change, caption = "Best Forecast AICc to measure change, using Stochastic") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot_AICc_Change <- Best_Model_AICc_Change %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Stochastic AICc model forecast of Change") +
      ggplot2::ylab("Total Change")

    Best_Innovation_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Date, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = "Stochastic AICc to measure change, innovation residuals")

    Best_ACF_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = "Stochastic AICc to measure change, Autocorrelation function")

    Best_Histogram_of_Residuals_AICc_Change <- fabletools::augment(Best_Model_AICc_Change) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Stochastic histogram of residuals")

    Best_Actual_vs_Predicted_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Stochastic AICc to measure change, Actual vs Predicted") +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals_AICc_Change <-
      ggplot2::ggplot(fabletools::augment(Best_Model_AICc_Change) %>% drop_na(), mapping = aes(x = Change, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Stochastic AICc to measure change, Actual vs Residuals") +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  forecast_accuracy_AICc_table

  forecast_accuracy_AICc_change_table

  forecast_accuracy

  return(list(
    plot_of_value, table_of_value_head, table_of_value_tail, plot_of_trend, plot_of_seasonally_adjusted, plot_of_decomposition, time_series_anomalies,
    plot_of_individual_seasons, plot_of_subseasons, plot_of_multiple_lags, Best_Model_AICc, Best_Forecast_AICc, Best_Forecast_plot_AICc, Best_Innovation_Residuals_AICc,
    Best_ACF_AICc, Best_Histogram_of_Residuals_AICc, Best_Actual_vs_Predicted_AICc, Best_Actual_vs_Residuals_AICc,
    Best_Model_AICc_Change, Best_Forecast_AICc_Change, Best_Forecast_plot_AICc_Change, Best_ACF_AICc_Change,
    Best_Histogram_of_Residuals_AICc_Change, Best_Actual_vs_Predicted_AICc_Change, Best_Actual_vs_Residuals_AICc_Change,
    forecast_accuracy_AICc_table, forecast_accuracy_AICc_change_table, forecast_accuracy
  ))
}
