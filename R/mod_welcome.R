#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 6, 3),
    row_heights = c(5, 2, 5),
    shiny::column(12),
      shiny::tagList(
        shiny::column(12),
        bslib::card(
          height = "50px",
          bslib::card_header("Welcome to FoodplanneR",align = "center"),

          shiny::h5("Press the button to begin"),
          shinyWidgets::actionBttn(ns("enter"),
                                   style = "fill",
                                   label = "Launch"),
          align = "center"
        ),
        shiny::column(12)
      ),
      shiny::column(12)

  )

}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
