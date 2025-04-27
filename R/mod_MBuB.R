#' bottom_up_beta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom magrittr %>%
#' @importFrom dplyr select

mod_fit_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::textInput(ns("company_input"),
                     "Input Company"),

    shiny::textInput(ns("index_input"),
                     "Input Index"),

    shiny::actionButton(ns("sp500_button"), "S&P500"),
    shiny::actionButton(ns("tsx_button"), "TSX"),
    shiny::actionButton(ns("hsi_button"), "HSI"),
    shiny::actionButton(ns("acwi_button"), "ACWI"),

    shiny::selectInput(ns("frequency_input"),
                       "Frequency",
                       c("Monthly", "Weekly")),

    shiny::selectInput(ns("years_input"),
                       "years",
                       c("2Y", "5Y")),

    shiny::actionButton(ns("grab_beta"),
                        "Grab Beta")

  )
}

#' bottom_up_beta Server Functions
#'
#' @noRd
mod_fit_regression_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$grab_beta, {
      req(input$company_input, input$index_input)

      beta_results <- uni_regression(r, input$company_input, input$index_input, input$frequency_input, switch(input$years_input, "2Y" = 2, "5Y" = 5))

      uni_regression(
        r,
        input$company_input,
        input$index_input,
        input$frequency_input,
        switch(input$years_input, "2Y" = 2, "5Y" = 5)
      )

      ggplot_regression(r)
      rolling_beta_plot(r)
    })
    observeEvent(input$sp500_button, {
      updateTextInput(session, "index_input", value = "^SPX")
    })
    observeEvent(input$tsx_button, {
      updateTextInput(session, "index_input", value = "^GSPTSE")
    })
    observeEvent(input$hsi_button, {
      updateTextInput(session, "index_input", value = "^HSI")
    })
    observeEvent(input$acwi_button, {
      updateTextInput(session, "index_input", value = "ACWI")
    })
  })
}
