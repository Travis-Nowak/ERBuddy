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
    })
  })
}
