#' financial_statements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_financial_statements_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("ticker"), "Company Ticker"),
        passwordInput(ns("api_key"), "FMP API Key"),
        actionButton(ns("fetch"), "Fetch Financials"),
        br(), br(),
        radioButtons(
          ns("scale"),
          "Display Units:",
          choices = c("Dollars" = 1, "Thousands" = 1000, "Millions" = 1e6),
          selected = 1,
          inline = TRUE
        ),
        br(),
        br(),
        downloadButton(ns("download_excel"), "Download All as Excel")
      ),
      mainPanel(
        h4("Income Statement"),
        uiOutput(ns("income_statement")),
        br(),
        h4("Balance Sheet"),
        uiOutput(ns("balance_sheet")),
        br(),
        h4("Cash Flow Statement"),
        uiOutput(ns("cash_flow"))

      )
    )
  )
}


#' financial_statements Server Functions
#'
#' @noRd
mod_financial_statements_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Local storage of data
    statements <- reactiveValues(
      income = NULL,
      balance = NULL,
      cashflow = NULL
    )

    get_fmp_data <- function(endpoint, ticker, api_key) {
      url <- paste0("https://financialmodelingprep.com/api/v3/", endpoint, "/", ticker, "?limit=10&apikey=", api_key)
      tryCatch({
        jsonlite::fromJSON(url)
      }, error = function(e) {
        return(NULL)
      })
    }

    observeEvent(input$fetch, {
      req(input$ticker, input$api_key)

      income <- get_fmp_data("income-statement", input$ticker, input$api_key)
      balance <- get_fmp_data("balance-sheet-statement", input$ticker, input$api_key)
      cashflow <- get_fmp_data("cash-flow-statement", input$ticker, input$api_key)

      # Save raw
      statements$income <- income
      statements$balance <- balance
      statements$cashflow <- cashflow

      # Render income
      if (!is.null(income)) {
        line_item_labels_is <- tibble::tibble(
          raw = c("revenue",
                  "costOfRevenue",
                  "grossProfit",
                  "researchAndDevelopmentExpenses",
                  #"generalAndAdministrativeExpenses",
                  #"sellingAndMarketingExpenses",
                  "sellingGeneralAndAdministrativeExpenses",
                  "otherExpenses",
                  #"operatingExpenses",
                  #"costAndExpenses",
                  "netInterestIncome",
                  "interestIncome",
                  "interestExpense",
                  "ebitda",
                  "depreciationAndAmortization",
                  "ebit",
                  "nonOperatingIncomeExcludingInterest",
                  "operatingIncome",
                  "totalOtherIncomeExpensesNet",
                  "incomeBeforeTax",
                  "incomeTaxExpense",
                  "netIncomeFromContinuingOperations",
                  "netIncomeFromDiscontinuedOperations",
                  "otherAdjustmentsToNetIncome",
                  "netIncome",
                  "netIncomeDeductions",
                  "bottomLineNetIncome",
                  "eps",
                  "epsDiluted",
                  "weightedAverageShsOut",
                  "weightedAverageShsOutDil"),
          label = c("Revenue",
                    "Cost of Revenue",
                    "Gross Profit",
                    "Research and Development Expenses",
                    #"General and Administrative Expenses",
                    #"Selling and Marketing Expenses",
                    "Selling, General and Administrative Expenses",
                    "Other Expenses",
                    #"Operating Expenses",
                    #"Cost and Expenses",
                    "Net Interest Income",
                    "Interest Income",
                    "Interest Expense",
                    "EBITDA",
                    "Depreciation and Amortization",
                    "EBIT",
                    "Non-Operating Income (Excluding Interest)",
                    "Operating Income",
                    "Total Other Income/Expenses (Net)",
                    "Income Before Tax",
                    "Income Tax Expense",
                    "Net Income from Continuing Operations",
                    "Net Income from Discontinued Operations",
                    "Other Adjustments to Net Income",
                    "Net Income",
                    "Net Income Deductions",
                    "Bottom Line Net Income",
                    "EPS",
                    "EPS Diluted",
                    "Weighted Average Shares Outstanding",
                    "Weighted Average Shares Outstanding (Diluted)"),
          is_major = raw %in% c(
            "revenue",
            "grossProfit",
            "operatingIncome",
            "incomeBeforeTax",
            "netIncome",
            "bottomLineNetIncome",
            "eps",
            "epsDiluted"
          ),

          skip_scaling = raw %in% c(
            "eps", "epsDiluted",
            "weightedAverageShsOut", "weightedAverageShsOutDil"
          )

        )

        output$income_statement <- renderUI({
          df <- as.data.frame(statements$income)

          if (is.null(df)) return(NULL)

          df_clean <- df %>%
            dplyr::select(date, any_of(line_item_labels_is$raw)) %>%
            tidyr::pivot_longer(-date, names_to = "raw", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = date, values_from = Value) %>%
            left_join(line_item_labels_is, by = "raw") %>%
            mutate(
              Metric = ifelse(is_major,
                              paste0("<strong>", label, "</strong>"),
                              paste0("&nbsp;&nbsp;&nbsp;&nbsp;", label))
            ) %>%
            {
              scale_val <- as.numeric(input$scale)
              mutate(., across(
                .cols = where(is.numeric),
                .fns = ~ ifelse(raw %in% line_item_labels_is$raw[line_item_labels_is$skip_scaling], ., . / scale_val)
              ))
            } %>%
            select(Metric, where(is.numeric))

          # Turn into HTML table
          htmltools::tagList(
            htmltools::tags$table(
              class = "table table-striped",
              htmltools::tags$thead(
                htmltools::tags$tr(
                  lapply(colnames(df_clean), function(col) htmltools::tags$th(HTML(col)))
                )
              ),
              htmltools::tags$tbody(
                apply(df_clean, 1, function(row) {
                  htmltools::tags$tr(
                    lapply(seq_along(row), function(i) {
                      cell <- row[[i]]
                      is_eps_row <- grepl("EPS", row[["Metric"]], ignore.case = TRUE)

                      if (!is.na(suppressWarnings(as.numeric(cell)))) {
                        digits <- if (is_eps_row) 2 else 0
                        formatted <- format(round(as.numeric(cell), digits),
                                            big.mark = ",", nsmall = digits, scientific = FALSE, trim = TRUE)
                      } else {
                        formatted <- cell
                      }
                      htmltools::tags$td(HTML(formatted))
                    })



                  )
                })
              )
            )
          )
        })
      }

      # Render balance
      if (!is.null(balance)) {
        line_item_labels_bs <- tibble::tibble(
          raw = c("cashAndCashEquivalents",
                  "shortTermInvestments",
                  #"cashAndShortTermInvestments",
                  "netReceivables",
                  "accountsReceivables",
                  "otherReceivables",
                  "inventory",
                  "prepaids",
                  "otherCurrentAssets",
                  "totalCurrentAssets",
                  "propertyPlantEquipmentNet",
                  "goodwill",
                  "intangibleAssets",
                  #"goodwillAndIntangibleAssets",
                  "longTermInvestments",
                  "taxAssets",
                  "otherNonCurrentAssets",
                  "totalNonCurrentAssets",
                  #"otherAssets",
                  "totalAssets",
                  "totalPayables",
                  "accountPayables",
                  "otherPayables",
                  "accruedExpenses",
                  "shortTermDebt",
                  #"capitalLeaseObligationsCurrent",
                  "taxPayables",
                  "deferredRevenue",
                  "otherCurrentLiabilities",
                  "totalCurrentLiabilities",
                  "longTermDebt",
                  "deferredRevenueNonCurrent",
                  "deferredTaxLiabilitiesNonCurrent",
                  "otherNonCurrentLiabilities",
                  "totalNonCurrentLiabilities",
                  #"otherLiabilities",
                  #"capitalLeaseObligations",
                  "totalLiabilities",
                  "treasuryStock",
                  "preferredStock",
                  "commonStock",
                  "retainedEarnings",
                  "additionalPaidInCapital",
                  "accumulatedOtherComprehensiveIncomeLoss",
                  "othertotalStockholdersEquity",
                  "totalStockholdersEquity",
                  #"totalEquity",
                  #"minorityInterest",
                  "totalLiabilitiesAndTotalEquity",
                  #"totalInvestments",
                  "totalDebt",
                  "netDebt"),
          label = c("Cash and Cash Equivalents",
                     "Short Term Investments",
                     #"Cash and Short Term Investments",
                     "Net Receivables",
                     "Accounts Receivables",
                     "Other Receivables",
                     "Inventory",
                     "Prepaids",
                     "Other Current Assets",
                     "Total Current Assets",
                     "Property Plant and Equipment (Net)",
                     "Goodwill",
                     "Intangible Assets",
                     #"Goodwill and Intangible Assets",
                     "Long Term Investments",
                     "Tax Assets",
                     "Other Non-Current Assets",
                     "Total Non-Current Assets",
                     #"Other Assets",
                     "Total Assets",
                     "Total Payables",
                     "Account Payables",
                     "Other Payables",
                     "Accrued Expenses",
                     "Short Term Debt",
                     #"Capital Lease Obligations (Current)",
                     "Tax Payables",
                     "Deferred Revenue",
                     "Other Current Liabilities",
                     "Total Current Liabilities",
                     "Long Term Debt",
                     "Deferred Revenue (Non-Current)",
                     "Deferred Tax Liabilities (Non-Current)",
                     "Other Non-Current Liabilities",
                     "Total Non-Current Liabilities",
                     #"Other Liabilities",
                     #"Capital Lease Obligations",
                     "Total Liabilities",
                     "Treasury Stock",
                     "Preferred Stock",
                     "Common Stock",
                     "Retained Earnings",
                     "Additional Paid-In Capital",
                     "Accumulated Other Comprehensive Income (Loss)",
                     "Other Total Stockholders' Equity",
                     "Total Stockholders' Equity",
                     #"Total Equity",
                     #"Minority Interest",
                     "Total Liabilities and Total Equity",
                     #"Total Investments",
                     "Total Debt",
                     "Net Debt"),
          is_major = raw %in% c(
            #"cashAndCashEquivalents",
            "totalCurrentAssets",
            #"propertyPlantEquipmentNet",
            "totalNonCurrentAssets",
            "totalAssets",
            "totalPayables",
            "totalCurrentLiabilities",
            "totalNonCurrentLiabilities",
            "totalLiabilities",
            "totalStockholdersEquity",
            #"totalEquity",
            "totalLiabilitiesAndTotalEquity",
            "totalDebt",
            "netDebt"
          )
        )


        output$balance_sheet <- renderUI({
          df <- as.data.frame(statements$balance)

          if (is.null(df)) return(NULL)

          df_clean <- df %>%
            dplyr::select(date, any_of(line_item_labels_bs$raw)) %>%
            tidyr::pivot_longer(-date, names_to = "raw", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = date, values_from = Value) %>%
            left_join(line_item_labels_bs, by = "raw") %>%
            mutate(
              Metric = ifelse(is_major,
                              paste0("<strong>", label, "</strong>"),
                              paste0("&nbsp;&nbsp;&nbsp;&nbsp;", label))
            ) %>%
            {
              scale_val <- as.numeric(input$scale)
              mutate(., across(where(is.numeric), ~ . / scale_val))
            } %>%
            select(Metric, where(is.numeric)) %>%
            {
              # Create a blank major row for "Current Assets"
              current_assets_row <- tibble::tibble(
                Metric = "<strong>Current Assets</strong>",
                !!!setNames(rep(NA_real_, ncol(.) - 1), names(.)[-1])
              )

              # Insert it at the top
              df <- bind_rows(current_assets_row, .)

              # Create a blank major row for "Current Liabilities"
              current_liabilities_row <- tibble::tibble(
                Metric = "<strong>Current Liabilities</strong>",
                !!!setNames(rep(NA_real_, ncol(.) - 1), names(.)[-1])
              )

              # Insert after "Total Assets"
              ta_index <- which(df$Metric == "<strong>Total Assets</strong>")
              if (length(ta_index) == 1) {
                df <- bind_rows(
                  df[1:ta_index, ],
                  current_liabilities_row,
                  df[(ta_index + 1):nrow(df), ]
                )
              }
            }



          # Turn into HTML table
          htmltools::tagList(
            htmltools::tags$table(
              class = "table table-striped",
              htmltools::tags$thead(
                htmltools::tags$tr(
                  lapply(colnames(df_clean), function(col) htmltools::tags$th(HTML(col)))
                )
              ),
              htmltools::tags$tbody(
                apply(df_clean, 1, function(row) {
                  htmltools::tags$tr(
                    lapply(seq_along(row), function(i) {
                      cell <- row[[i]]
                      is_eps_row <- grepl("EPS", row[["Metric"]], ignore.case = TRUE)

                      numeric_cell <- suppressWarnings(as.numeric(cell))
                      if (!is.na(numeric_cell)) {
                        digits <- if (is_eps_row) 2 else 0
                        formatted <- format(round(numeric_cell, digits),
                                            big.mark = ",", nsmall = digits, scientific = FALSE, trim = TRUE)
                      } else if (i == 1) {
                        # first column is the metric label, already HTML-safe
                        formatted <- cell
                      } else {
                        # empty string for missing numeric values
                        formatted <- ""
                      }

                      htmltools::tags$td(HTML(formatted))
                    })
                  )
                })
              )
            )
          )
        })

      # Render cash flow
      if (!is.null(cashflow)) {
        line_item_labels_cf <- tibble::tibble(
          raw = c(
            "netIncome",
            "depreciationAndAmortization",
            "deferredIncomeTax",
            "stockBasedCompensation",
            "otherNonCashItems",
            #"changeInWorkingCapital",
            "accountsReceivables",
            "inventory",
            "accountsPayables",
            "otherWorkingCapital",
            "netCashProvidedByOperatingActivities",


            # INVESTING
            "investmentsInPropertyPlantAndEquipment",
            "acquisitionsNet",
            "purchasesOfInvestments",
            "salesMaturitiesOfInvestments",
            "otherInvestingActivites",
            "netCashProvidedByInvestingActivities",
            "netCashUsedForInvestingActivites",


            # FINANCING
            "debtRepayment",
            "commonStockIssued",
            "commonStockRepurchased",
            "dividendsPaid",
            "otherFinancingActivites",
            "netCashUsedProvidedByFinancingActivities",

            # OTHER
            "effectOfForexChangesOnCash",
            "cashAtBeginningOfPeriod",
            "netChangeInCash",
            "cashAtEndOfPeriod",
            #"operatingCashFlow",
            #"capitalExpenditure",
            "freeCashFlow",
            "incomeTaxesPaid",
            "interestPaid"
          ),
          label = c(
            "Net Income",
            "Depreciation and Amortization",
            "Deferred Income Tax",
            "Stock-Based Compensation",
            "Other Non-Cash Charges",
            #"Change in Working Capital",
            "Accounts Receivables",
            "Inventory",
            "Accounts Payables",
            "Other Working Capital",
            "Net Cash Provided by Operating Activities",

            # INVESTING
            "Investments in Property, Plant and Equipment",
            "Acquisitions (Net)",
            "Purchases of Investments",
            "Sales/Maturities of Investments",
            "Other Investing Activities",
            "Net Cash Provided by Investing Activities",
            "Net Cash Used by Investing Activities",

            # FINANCING
            "Debt Repayment",
            "Common Stock Issued",
            "Common Stock Repurchased",
            "Dividends Paid",
            "Other Financing Activities",
            "Net Cash Provided by Financing Activities",

            # OTHER
            "Effect of Forex Changes on Cash",
            "Cash at Beginning of Period",
            "Net Change in Cash",
            "Cash at End of Period",
            #"Operating Cash Flow",
            #"Capital Expenditure",
            "Free Cash Flow",
            "Income Taxes Paid",
            "Interest Paid"
          ),
          is_major = raw %in% c(
            "netIncome",
            "netCashProvidedByOperatingActivities",
            "netCashProvidedByInvestingActivities",
            "netCashProvidedByFinancingActivities",
            "netCashUsedProvidedByFinancingActivities",
            "netCashUsedForInvestingActivites",
            "netChangeInCash",
            "cashAtEndOfPeriod",
            "cashAtBeginningOfPeriod",
            "freeCashFlow"
          )
        )



        output$cash_flow <- renderUI({
          df <- as.data.frame(statements$cashflow)
          if (is.null(df)) return(NULL)

          df_clean <- df %>%
            dplyr::select(date, any_of(line_item_labels_cf$raw)) %>%
            tidyr::pivot_longer(-date, names_to = "raw", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = date, values_from = Value) %>%
            left_join(line_item_labels_cf, by = "raw") %>%
            mutate(
              Metric = ifelse(is_major,
                              paste0("<strong>", label, "</strong>"),
                              paste0("&nbsp;&nbsp;&nbsp;&nbsp;", label))
            ) %>%
            {
              scale_val <- as.numeric(input$scale)
              mutate(., across(where(is.numeric), ~ . / scale_val))
            } %>%
            select(Metric, where(is.numeric)) %>%
            {
              df <- .

              # Insert "Adjustments" at the top
              adjustments_row <- tibble::tibble(
                Metric = "<strong>Adjustments to Reconcile Net Income to Cash</strong>",
                !!!setNames(rep(NA_real_, ncol(df) - 1), names(df)[-1])
              )
              df <- bind_rows(adjustments_row, df)

              # Insert "Changes in Operating Assets and Liabilities" after Other Non-Cash Charges
              insert_after <- which(grepl("Other Non-Cash Charges", df$Metric, fixed = TRUE))
              if (length(insert_after) == 1) {
                wc_row <- tibble::tibble(
                  Metric = "<strong>Changes in Operating Assets and Liabilities</strong>",
                  !!!setNames(rep(NA_real_, ncol(df) - 1), names(df)[-1])
                )
                df <- bind_rows(
                  df[1:insert_after, ],
                  wc_row,
                  df[(insert_after + 1):nrow(df), ]
                )
              }

              # Insert "Investing Activities" after Net Cash from Ops
              ncfo_index <- which(df$Metric == "<strong>Net Cash Provided by Operating Activities</strong>")
              if (length(ncfo_index) == 1) {
                investing_header <- tibble::tibble(
                  Metric = "<strong>Investing Activities</strong>",
                  !!!setNames(rep(NA_real_, ncol(df) - 1), names(df)[-1])
                )
                df <- bind_rows(
                  df[1:ncfo_index, ],
                  investing_header,
                  df[(ncfo_index + 1):nrow(df), ]
                )
              }

              # Insert "Financing Activities" after Sales/Maturities or Other Investing Line
              last_investing_row <- which(grepl("Sales/Maturities of Investments|Net Cash Used by Investing Activities", df$Metric))
              if (length(last_investing_row) > 0) {
                last_inv <- max(last_investing_row)
                financing_header <- tibble::tibble(
                  Metric = "<strong>Financing Activities</strong>",
                  !!!setNames(rep(NA_real_, ncol(df) - 1), names(df)[-1])
                )
                df <- bind_rows(
                  df[1:last_inv, ],
                  financing_header,
                  df[(last_inv + 1):nrow(df), ]
                )
              }


              df
            } -> df_clean

          # Render as HTML table
          htmltools::tagList(
            htmltools::tags$table(
              class = "table table-striped",
              htmltools::tags$thead(
                htmltools::tags$tr(
                  lapply(colnames(df_clean), function(col) htmltools::tags$th(HTML(col)))
                )
              ),
              htmltools::tags$tbody(
                apply(df_clean, 1, function(row) {
                  htmltools::tags$tr(
                    lapply(seq_along(row), function(i) {
                      cell <- row[[i]]
                      is_eps_row <- grepl("EPS", row[["Metric"]], ignore.case = TRUE)

                      numeric_cell <- suppressWarnings(as.numeric(cell))
                      if (!is.na(numeric_cell)) {
                        digits <- if (is_eps_row) 2 else 0
                        formatted <- format(round(numeric_cell, digits),
                                            big.mark = ",", nsmall = digits, scientific = FALSE, trim = TRUE)
                      } else if (i == 1) {
                        formatted <- cell
                      } else {
                        formatted <- ""
                      }

                      htmltools::tags$td(HTML(formatted))
                    })
                  )
                })
              )
            )
          )
        })


      }

        #print(names(statements$cashflow))


    # Download Handler
        output$download_excel <- downloadHandler(
          filename = function() {
            paste0(input$ticker, "_financials_long_format.xlsx")
          },
          content = function(file) {
            if (is.null(statements$income) || is.null(statements$balance) || is.null(statements$cashflow)) {
              return(NULL)
            }

            # Convert each statement to long format
            to_long <- function(df, label_df) {
              df %>%
                dplyr::select(date, any_of(label_df$raw)) %>%
                tidyr::pivot_longer(-date, names_to = "raw", values_to = "value") %>%
                left_join(label_df, by = "raw") %>%
                dplyr::select(date, line_item = label, value)
            }

            long_income <- to_long(statements$income, line_item_labels_is)
            long_balance <- to_long(statements$balance, line_item_labels_bs)
            long_cashflow <- to_long(statements$cashflow, line_item_labels_cf)

            openxlsx::write.xlsx(
              list(
                Income_Statement = long_income,
                Balance_Sheet = long_balance,
                Cash_Flow = long_cashflow
              ),
              file = file
            )
          }
        )
      }
    })
  })
}
