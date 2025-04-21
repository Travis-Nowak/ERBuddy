#' uni_regression
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
uni_regression <- function(r, company_ticker, index_ticker, frequency, years_back) {


  from_date <- lubridate::floor_date(Sys.Date() - lubridate::years(years_back),
                                     unit = ifelse(frequency == "Monthly", "month", "week"))

  ifelse(
    frequency == "Monthly",
  to_date <- lubridate::rollback(Sys.Date(), roll_to_first = TRUE) - 1,
  to_date <- Sys.Date() - lubridate::wday(Sys.Date(), week_start = 1) - 2
  )

  combined_tickers <- c(company_ticker, index_ticker)

  # Pull prices
  prices <- tidyquant::tq_get(combined_tickers, get = "stock.prices", from = from_date, to = to_date)


  # Get periodic returns
  period_type <- tolower(frequency)

  prices_periodic <- prices %>%
    group_by(symbol) %>%
    tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = period_type,
      type       = "arithmetic",
      col_rename = "returns"
    )


  # Only keep latest 60 observations if enough data exists
  expected_periods <- switch(frequency,
                             "Monthly" = 12,
                             "Weekly" = 52) * years_back


  prices_periodic <- prices_periodic %>%
    group_by(symbol) %>%
    arrange(date) %>%
    filter(n() >= expected_periods) %>%
    slice_tail(n = expected_periods)


  #print(sapply(prices_periodic, summary))

  #print("----")

  #print(head(prices_periodic))

  prices_periodic_wide <- prices_periodic %>%
    pivot_wider(
      id_cols = date, names_from = symbol, values_from = returns
    )

  #print("----")

  #print(head(prices_periodic_wide))
  #print(tail(prices_periodic_wide))

  #print(names(prices_periodic_wide))

  names(prices_periodic_wide)[3] <- "market"

  # Dynamically set dependent and independent variables
  y_var <- names(prices_periodic_wide)[2]  # 2nd column (dependent)
  x_var <- names(prices_periodic_wide)[3]  # 3rd column (independent)


  # Create formula for model
  formula <- reformulate(termlabels = x_var, response = y_var)

  # Fit model
  model <- lm(formula, data = prices_periodic_wide)



  # Return beta
  beta <- as.numeric(model$coefficients[2])

  #print(beta)

  r$beta_results <- beta
  r$beta_table <- prices_periodic_wide

}
