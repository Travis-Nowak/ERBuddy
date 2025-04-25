#' rolling_beta_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
rolling_beta_plot <- function(r, window = 12) {
  req(r$uni_ticker, r$uni_benchmark, r$uni_freq, r$uni_timeline)

  company_ticker <- r$uni_ticker
  index_ticker   <- r$uni_benchmark
  frequency      <- r$uni_freq
  years_back     <- r$uni_timeline

  window <- years_back * switch(frequency, "Monthly" = 12, "Weekly" = 52)

  from_date <- lubridate::floor_date(Sys.Date() - lubridate::years(2 * years_back),
                                     unit = ifelse(frequency == "Monthly", "month", "week"))


  to_date <- if (frequency == "Monthly") {
    lubridate::rollback(Sys.Date(), roll_to_first = TRUE) - 1
  } else {
    Sys.Date() - lubridate::wday(Sys.Date(), week_start = 1) - 2
  }

  combined_tickers <- c(company_ticker, index_ticker)

  # Pull price data
  prices <- tidyquant::tq_get(combined_tickers, get = "stock.prices", from = from_date, to = to_date)

  if (nrow(prices) == 0 || !(company_ticker %in% unique(prices$symbol))) {
    r$rolling_beta_plot <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = paste0("Not enough data to calculate rolling beta for ", company_ticker),
                        size = 5, hjust = 0.5)
    return(invisible(NULL))
  }

  # Get periodic returns
  period_type <- tolower(frequency)

  lookback_period <- 2 * window

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
    filter(n() >= lookback_period) %>%
    slice_tail(n = lookback_period) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = symbol, values_from = returns)

  # Always rename explicitly based on tickers (safe)
  available_cols <- colnames(prices_periodic)

  if (!(company_ticker %in% available_cols) || !(index_ticker %in% available_cols)) {
    r$rolling_beta_plot <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = paste0("Not enough return data to calculate rolling beta for ", company_ticker),
                        size = 5, hjust = 0.5)
    return(invisible(NULL))
  }


  prices_periodic <- prices_periodic %>%
    dplyr::rename(
      stock_return = !!rlang::sym(company_ticker),
      market_return = !!rlang::sym(index_ticker)
    )



  names(prices_periodic)[2:3] <- c("stock_return", "market_return")

  # Rolling regression to calculate beta
  roll_beta <- slider::slide_dfr(
    .x = seq_len(nrow(prices_periodic) - window + 1),
    .f = ~ {
      data_window <- prices_periodic[.x:(.x + window - 1), ]
      model <- lm(stock_return ~ market_return, data = data_window)
      summary_model <- summary(model)

      tibble::tibble(
        date = max(data_window$date),
        beta = coef(model)[2],
        r_squared = summary_model$r.squared
      )
    }
  )

  # Replace just the plotting section:
  last_point <- roll_beta %>% slice_tail(n = 1)

  library(patchwork)  # install.packages("patchwork") if needed

  # Plot 1: Beta
  p1 <- ggplot2::ggplot(roll_beta, ggplot2::aes(x = date, y = beta)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
    ggrepel::geom_text_repel(
      data = roll_beta %>% slice_tail(n = 1),
      ggplot2::aes(label = paste0("β = ", round(beta, 2))),
      nudge_y = 0.1,
      size = 4,
      color = "black"
    ) +
    ggplot2::labs(
      title = paste0("Rolling ", window, "-Period Beta (", frequency, ")"),
      subtitle = paste0(company_ticker, " vs ", index_ticker),
      y = "Rolling Beta", x = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                             axis.ticks.x = ggplot2::element_blank())

  # Plot 2: R²
  p2 <- ggplot2::ggplot(roll_beta, ggplot2::aes(x = date, y = r_squared)) +
    ggplot2::geom_line(color = "darkgreen", size = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "forestgreen") +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      y = expression(R^2),
      x = "Date"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(
      date_breaks = "2 months",         # Customize as needed
      date_labels = "%b %Y"             # E.g., "Jan 2022"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


  # Combine both using patchwork
  r$rolling_beta_plot <- p1 / p2 + patchwork::plot_layout(heights = c(2, 1))

}
