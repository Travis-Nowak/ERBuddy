#' financial_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_financial_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' financial_trends Server Functions
#'
#' @noRd 
mod_financial_trends_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_financial_trends_ui("financial_trends_1")
    
## To be copied in the server
# mod_financial_trends_server("financial_trends_1")
