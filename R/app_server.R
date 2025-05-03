#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues()

  mod_fit_regression_server("univariate_regression", r)
  mod_show_regression_server("show_regression", r)
  #mod_MBuB_server("mbub_inputs", r)
  mod_financial_statements_server("financial_statements_1")


}
