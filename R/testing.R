library(dplyr)
library(tidyr)
library(tidyquant)

company_ticker <- "TEX"

index_ticker <- "^GSPC"

combined_tickers <- c(company_ticker, index_ticker)

# grab price returns
prices <- tidyquant::tq_get(combined_tickers, get = "stock.prices", from = "2020-01-01")


# convert to monthly
monthly_returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = to.monthly,
    indexAt = "lastof",
    col_rename = "monthly_adjusted") %>%
  mutate(
    returns = (monthly_adjusted / lag(monthly_adjusted)) - 1) %>% # convert to returns
  tidyr::drop_na() %>%
  select("symbol", "date", "returns")

returns_wide <- monthly_returns %>%
  pivot_wider(id_cols = date, names_from = symbol, values_from = returns)

names(returns_wide)[3] <- "market"

# Dynamically set dependent and independent variables
y_var <- names(returns_wide)[2]  # 2nd column (dependent)
x_var <- names(returns_wide)[3]  # 3rd column (independent)

# Create formula
formula <- reformulate(termlabels = x_var, response = y_var)

# Fit model
model <- lm(formula, data = returns_wide)

beta <- as.numeric(model$coefficients[2])

