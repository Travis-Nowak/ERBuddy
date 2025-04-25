#' ggplot_regression
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
ggplot_regression <- function(r) {
  req(r$uni_ticker, r$uni_benchmark, r$uni_freq, r$uni_timeline)

  company_ticker <- r$uni_ticker
  index_ticker   <- r$uni_benchmark
  frequency      <- r$uni_freq
  years_back     <- r$uni_timeline

  from_date <- lubridate::floor_date(Sys.Date() - lubridate::years(years_back),
                                     unit = ifelse(frequency == "Monthly", "month", "week"))

  to_date <- if (frequency == "Monthly") {
    lubridate::rollback(Sys.Date(), roll_to_first = TRUE) - 1
  } else {
    Sys.Date() - lubridate::wday(Sys.Date(), week_start = 1) - 2
  }

  combined_tickers <- c(company_ticker, index_ticker)

  prices <- tidyquant::tq_get(combined_tickers, get = "stock.prices", from = from_date, to = to_date)

  period_type <- tolower(frequency)

  prices_periodic <- prices %>%
    group_by(symbol) %>%
    tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = period_type,
      type       = "arithmetic",
      col_rename = "returns"
    ) %>%
    group_by(symbol) %>%
    arrange(date) %>%
    filter(n() >= years_back * ifelse(frequency == "Monthly", 12, 52)) %>%
    slice_tail(n = years_back * ifelse(frequency == "Monthly", 12, 52))

  # Plot
  p <- ggplot2::ggplot(prices_periodic, ggplot2::aes(x = date, y = returns, color = symbol)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
      title = paste0(company_ticker, " vs ", index_ticker, " Returns"),
      x = "Date", y = "Periodic Return",
      color = "Ticker"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(
      date_breaks = "2 months",         # Customize as needed
      date_labels = "%b %Y"             # E.g., "Jan 2022"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


  r$return_plot <- p
}
