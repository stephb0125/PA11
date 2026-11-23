
# Stephanie Johnson
# COPC2073
# MODULE 11
# 11/22/26

# Create a tibble with cylinders and weight
cars_data <- tibble::as_tibble(mtcars) |>
  dplyr::select(cyl, wt) |>
  dplyr::mutate(cyl = as.factor(cyl))

# Fit a linear model to predict weight
cars_model <- lm(wt ~ cyl, data = cars_data)

# Create a data grid and add predictions
cars_predictions <- cars_data |>
  modelr::data_grid(cyl) |>
  modelr::add_predictions(cars_model)

# Plot original data and predicted values
ggplot2::ggplot(cars_data, ggplot2::aes(x = cyl, y = wt)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::geom_point(
    data = cars_predictions,
    mapping = ggplot2::aes(x = cyl, y = pred),
    color = "red",
    size = 4
  ) +
  ggplot2::labs(
    x = "cyl",
    y = "wt"
  ) +
  ggplot2::theme_gray()

# analysis: the plot shows a relationship between the number of cylinders
# and the weight of a car. The cars with more cylinders 
# weigh more than cars with fewer cylinders.
# The red points represent the predicted average weight for
# each cylinder category based on the linear model.
# Four cylinder cars will have the lowest predicted average weight.
# Eight cylinder cars have the highest predicted weight.
# The black points show the actual weights of individual cars.
# Cylinder count is useful for predicting the car weight,
# and weights vary within each cylinder category.
