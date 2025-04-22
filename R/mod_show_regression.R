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
  )
}

#' show_regression Server Functions
#'
#' @noRd
mod_show_regression_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_show_regression_ui("show_regression_1")

## To be copied in the server
# mod_show_regression_server("show_regression_1")
