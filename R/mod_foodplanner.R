#' foodplanner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_foodplanner_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::layout_column_wrap(
      width = 1/3,
      tagList(
                    bslib::card(bslib::card_header("Select days"),
                                shiny::sliderInput(
                                  inputId = ns("daysno"),
                                  min = 1,
                                  max = 5,
                                  label = "Select number of days for planning",
                                  value = 5,
                                  step = 1,
                                  ticks = TRUE
                                ))),
                    shinyWidgets::actionBttn(
                      inputId = ns("generate"),
                      label = "Generate my meal plan",
                      style = "jelly"
                    )),

    bslib::layout_column_wrap(
      width = 1/3,
        tagList(shiny::uiOutput(outputId = ns("day1")),
                    shiny::uiOutput(outputId = ns("day2"))),
        tagList(shiny::uiOutput(outputId = ns("day3")),
                    shiny::uiOutput(outputId = ns("day4"))),
        tagList(shiny::uiOutput(outputId = ns("day5"))
      ))
,
    bslib::layout_column_wrap(
      width = 1/2,

             bslib::card(
               bslib::card_header("Meal Plan"),
               DT::dataTableOutput(
                 outputId = ns("meal_overview")
               )
             ))
    )

}

#' foodplanner Server Functions
#'
#' @noRd
mod_foodplanner_server <- function(id, app_data,meal_plan, parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$day1 <- shiny::renderUI({

        bslib::card(
          bslib::card_header("Day 1"),
          shinyWidgets::sliderTextInput(
            inputId = ns("time1"),
            label = "Select preparation length range",
            choices = c("Short","Medium", "Long"),
            selected = "Medium",grid = T
          ),
          shinyWidgets::prettyCheckbox(
            inputId = ns("vegetarian1"),
            label = "Vegetarian?",
            value = T
          )
        )
    })

    output$day2 <- shiny::renderUI({
      if(input$daysno>1){
        bslib::card(
          bslib::card_header("Day 2"),
          shinyWidgets::sliderTextInput(
            inputId = ns("time2"),
            label = "Select preparation length range",
            choices = c("Short","Medium", "Long"),
            selected = "Medium",grid = T
          ),
          shinyWidgets::prettyCheckbox(
            inputId = ns("vegetarian2"),
            label = "Vegetarian?",
            value = T
          )
        )
      }
      else{

      }
    })

    output$day3 <- shiny::renderUI({
      if(input$daysno>2){
        bslib::card(
          bslib::card_header("Day 3"),
          shinyWidgets::sliderTextInput(
            inputId = ns("time3"),
            label = "Select preparation length range",
            choices = c("Short","Medium", "Long"),
            selected = "Medium",grid = T
          ),
          shinyWidgets::prettyCheckbox(
            inputId = ns("vegetarian3"),
            label = "Vegetarian?",
            value = T
          )
        )
      }
      else{

      }
    })

    output$day4 <- shiny::renderUI({
      if(input$daysno>3){
        bslib::card(
          bslib::card_header("Day 4"),
          shinyWidgets::sliderTextInput(
            inputId = ns("time4"),
            label = "Select preparation length range",
            choices = c("Short","Medium", "Long"),
            selected = "Medium",grid = T
          ),
          shinyWidgets::prettyCheckbox(
            inputId = ns("vegetarian4"),
            label = "Vegetarian?",
            value = T
          )
        )
      }
      else{

      }
    })

    output$day5 <- shiny::renderUI({
      if(input$daysno>4){
        bslib::card(
          bslib::card_header("Day 5"),
          shinyWidgets::sliderTextInput(
            inputId = ns("time5"),
            label = "Select preparation length range",
            choices = c("Short","Medium", "Long"),
            selected = "Medium",grid = T
          ),
          shinyWidgets::prettyCheckbox(
            inputId = ns("vegetarian5"),
            label = "Vegetarian?",
            value = T
          )
        )
      }
      else{

      }
    })

    shiny::observeEvent(input$generate,{
      meal_plan$core_data <- as.data.frame(matrix(ncol = 6,nrow = 0))
      colnames(meal_plan$core_data) <- c("Day" ,
                                         "Meal",
                                         "Time" ,
                                         "Vegetarian",
                                         "Groceries",
                                         "Recipe"
      )

      meal_plan$overview <- meal_plan$core_data


    })

  output$meal_overview <- DT::renderDataTable({
    if(!is.null(meal_plan$overview)){
      DT::datatable(meal_plan$overview)
    }
  })

  })
}

## To be copied in the UI
# mod_foodplanner_ui("foodplanner_1")

## To be copied in the server
# mod_foodplanner_server("foodplanner_1")
