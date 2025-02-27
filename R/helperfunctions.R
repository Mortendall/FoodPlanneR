select_meal <- function(app_data, time, vegetarian){
  if(time  =="Medium"){
    app_data <- app_data |>
      dplyr::filter(time !="Long")
  }
  if(time == "Short"){
    app_data <- app_data |>
      dplyr::filter(time =="Short")
  }
  if(isTRUE(vegetarian)){
    app_data <- app_data |>
      dplyr::filter(Vegetarian)
  }
}
