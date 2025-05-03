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
        downloadButton(ns("download_excel"), "Download All as Excel")
      ),
      mainPanel(
        h4("Income Statement"),
        tableOutput(ns("income_statement")),
        br(),
        h4("Balance Sheet"),
        tableOutput(ns("balance_sheet")),
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
        output$income_statement <- renderTable({
          as.data.frame(income) %>%
            dplyr::select(date, dplyr::any_of(c("revenue",
                                                "costOfRevenue",
                                                "grossProfit",
                                                "researchAndDevelopmentExpenses",
                                                "generalAndAdministrativeExpenses",
                                                "sellingAndMarketingExpenses",
                                                "sellingGeneralAndAdministrativeExpenses",
                                                "otherExpenses",
                                                "operatingExpenses",
                                                "costAndExpenses",
                                                "netInterestIncome",
                                                "interestIncome",
                                                "interestExpense",
                                                "depreciationAndAmortization",
                                                "ebitda",
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
                                                "weightedAverageShsOutDil"))) %>%
            tidyr::pivot_longer(-date, names_to = "Metric", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = date, values_from = Value)
        })
      }

      # Render balance
      if (!is.null(balance)) {
        output$balance_sheet <- renderTable({
          as.data.frame(balance) %>%
            dplyr::select(date, dplyr::any_of(c(
                "cashAndCashEquivalents",
                "shortTermInvestments",
                "cashAndShortTermInvestments",
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
                "goodwillAndIntangibleAssets",
                "longTermInvestments",
                "taxAssets",
                "otherNonCurrentAssets",
                "totalNonCurrentAssets",
                "otherAssets",
                "totalAssets",
                "totalPayables",
                "accountPayables",
                "otherPayables",
                "accruedExpenses",
                "shortTermDebt",
                "capitalLeaseObligationsCurrent",
                "taxPayables",
                "deferredRevenue",
                "otherCurrentLiabilities",
                "totalCurrentLiabilities",
                "longTermDebt",
                "deferredRevenueNonCurrent",
                "deferredTaxLiabilitiesNonCurrent",
                "otherNonCurrentLiabilities",
                "totalNonCurrentLiabilities",
                "otherLiabilities",
                "capitalLeaseObligations",
                "totalLiabilities",
                "treasuryStock",
                "preferredStock",
                "commonStock",
                "retainedEarnings",
                "additionalPaidInCapital",
                "accumulatedOtherComprehensiveIncomeLoss",
                "otherTotalStockholdersEquity",
                "totalStockholdersEquity",
                "totalEquity",
                "minorityInterest",
                "totalLiabilitiesAndTotalEquity",
                "totalInvestments",
                "totalDebt",
                "netDebt"))) %>%
            tidyr::pivot_longer(-date, names_to = "Metric", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = date, values_from = Value)
        })
      }

      # Render cash flow
      if (!is.null(cashflow)) {
        line_item_labels_cf <- tibble::tibble(
          raw = c("netIncome", "depreciationAndAmortization", "deferredIncomeTax", "stockBasedCompensation",
                  "changeInWorkingCapital", "accountsReceivables", "inventory", "accountsPayables",
                  "otherWorkingCapital", "otherNonCashIems", "netCashProvidedByOperatingActivities",
                  "investmentsInPropertyPlantAndEquipment", "acquisitionsNet", "purchasesOfInvestments",
                  "salesMaturitiesOfInvestments", "otherInvestingActivities", "netCashProvidedByInvestingActivities",
                  "netDebtIssuance", "longTermNetDebtIssuance", "shortTermNetDebtIssuance", "netStockIssuance",
                  "netCommonStockIssuance", "commonStockIssuance", "commonStockRepurchased",
                  "netPreferredStockIssuance", "netDividendsPaid", "commonDividendsPaid",
                  "preferredDividendsPaid", "otherFinancingActivities", "netCashProvidedByFinancingActivities",
                  "effectOfForexChangesOnCash", "netChangeInCash", "cashAtEndOfPeriod", "cashAtBeginningOfPeriod",
                  "operatingCashFlow", "capitalExpenditure", "freeCashFlow", "incomeTaxesPaid", "interestPaid"),
          label = c("Net Income", "Depreciation and Amortization", "Deferred Income Tax", "Stock-Based Compensation",
                    "Change in Working Capital", "Accounts Receivables", "Inventory", "Accounts Payables",
                    "Other Working Capital", "Other Non-Cash Items", "Net Cash Provided by Operating Activities",
                    "Investments in Property, Plant and Equipment", "Acquisitions (Net)", "Purchases of Investments",
                    "Sales/Maturities of Investments", "Other Investing Activities", "Net Cash Provided by Investing Activities",
                    "Net Debt Issuance", "Long-Term Net Debt Issuance", "Short-Term Net Debt Issuance", "Net Stock Issuance",
                    "Net Common Stock Issuance", "Common Stock Issuance", "Common Stock Repurchased",
                    "Net Preferred Stock Issuance", "Net Dividends Paid", "Common Dividends Paid",
                    "Preferred Dividends Paid", "Other Financing Activities", "Net Cash Provided by Financing Activities",
                    "Effect of Forex Changes on Cash", "Net Change in Cash", "Cash at End of Period", "Cash at Beginning of Period",
                    "Operating Cash Flow", "Capital Expenditure", "Free Cash Flow", "Income Taxes Paid", "Interest Paid"),
          is_major = raw %in% c(
            "netIncome",
            "netCashProvidedByOperatingActivities",
            "netCashProvidedByInvestingActivities",
            "netCashProvidedByFinancingActivities",
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
              Metric = ifelse(is_major, paste0("<strong>", label, "</strong>"),
                              paste0("&nbsp;&nbsp;&nbsp;&nbsp;", label))
            ) %>%
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
                    lapply(row, function(cell) htmltools::tags$td(HTML(format(cell, big.mark = ","))))
                  )
                })
              )
            )
          )
        })


    # Download Handler
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0(input$ticker, "_financials.xlsx")
      },
      content = function(file) {
        if (is.null(statements$income) || is.null(statements$balance) || is.null(statements$cashflow)) {
          return(NULL)
        }
        openxlsx::write.xlsx(
          list(
            Income_Statement = statements$income,
            Balance_Sheet = statements$balance,
            Cash_Flow = statements$cashflow
          ),
          file = file
        )
      }
    )
      }
    })
  })
}


## To be copied in the UI
# mod_financial_statements_ui("financial_statements_1")

## To be copied in the server
# mod_financial_statements_server("financial_statements_1")
