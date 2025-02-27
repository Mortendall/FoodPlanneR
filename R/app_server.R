#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  library(duckplyr)
  parentsession <- session
  app_data <- arrow::read_parquet(here::here("Data/maddata.parquet"))
  meal_plan <- shiny::reactiveValues()
  mod_welcome_server("welcome_1", app_data, parentsession)
  mod_foodplanner_server("foodplanner_1", app_data,meal_plan, parentsession)


  # Your application server logic
}
