#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "litera",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Raleway")
  )

  shiny::fluidPage(
    theme = theme,
    golem_add_external_resources(),
    shiny::titlePanel("Equity Research Buddy"),

    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Beta",

                      sidebarLayout(
                        sidebarPanel(
                      mod_fit_regression_ui("univariate_regression"),
                      width = 4
                        ),
                      mainPanel(
                        mod_show_regression_ui("show_regression"),
                        width = 8
                      )
                      )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ERBuddy"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
