plot_box <- function(data, ...) {
  # Creates a horizontal boxplot for each specified numeric variable.
  # Usage: plot_box(my_data, var1, var2, var3)
  # Note: all variables share a common x-axis, so this works best
  #       with variables on similar scales.

  data |>
    select(...) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value, y = variable)) +
    geom_boxplot() +
    labs(
      title = "Boxplots",
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
}
