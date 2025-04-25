#' show_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_show_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
      shiny::h4("Regression Summary"),
      shiny::tableOutput(ns("regression_table")),
      shiny::h4("Return Comparisson"),
      shiny::plotOutput(ns("return_plot"))
  )
}

#' show_regression Server Functions
#'
#' @noRd
mod_show_regression_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$regression_table <- renderTable({
      req(r$regression_stats)
      r$regression_stats
    }, digits = 4)

    output$return_plot <- renderPlot({
      req(r$return_plot)
      r$return_plot
    })


  })
}

## To be copied in the UI
# mod_show_regression_ui("show_regression_1")

## To be copied in the server
# mod_show_regression_server("show_regression_1")
