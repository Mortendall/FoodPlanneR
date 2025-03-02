#' Select meak
#'
#' @param app_data a data frame with all possible recipes
#' @param meal_plan a data frame with preset column names
#' @param time "Medium", "Short" or "Long"
#' @param vegetarian "y" or "n"
#' @param day_name character vector of the name you want for the picked entry
#'
#' @returns meal plan with an added row sampled from app_data

select_meal <- function(app_data,
                        meal_plan,
                        time,
                        vegetarian,
                        day_name) {
  if (time  == "Medium") {
    app_data <- app_data |>
      dplyr::filter(time != "Long")
  }
  if (time == "Short") {
    app_data <- app_data |>
      dplyr::filter(time == "Short")
  }
  if (isTRUE(vegetarian)) {
    app_data <- app_data |>
      dplyr::filter(Vegetarian == "y")
  }
  selected_meal <- dplyr::sample_n(app_data, 1) |>
    dplyr::mutate(Day = day_name)

  if (length(meal_plan$Day) == 0) {
    meal_plan <- selected_meal
  }
  else{
    meal_plan <- dplyr::rows_append(meal_plan, selected_meal)
  }

  return(meal_plan)
}
