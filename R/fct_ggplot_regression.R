#' ggplot_regression
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ggplot_regression <- function(r, company_ticker, index_ticker, frequency, years_back) {

  r$plot <- NULL

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


  prices_periodic_wide <- prices_periodic %>%
    pivot_wider(
      id_cols = date, names_from = symbol, values_from = returns
    )

  r$plot <- ggplot(
    aes(
      x = prices_periodic_wide[2], y = prices_periodic_wide[3]
    )
  ) +
    ggplot2::geom_line()

}
