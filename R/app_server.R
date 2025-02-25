#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  app_data <- arrow::read_parquet(here::here("Data/maddata.parquet"))
  mod_welcome_server("welcome_1")
  mod_foodplanner_server("foodplanner_1", app_data)


  # Your application server logic
}
