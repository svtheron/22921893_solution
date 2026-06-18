plot_ridges <- function(data, ..., fill = "steelblue") {
  # Creates a density ridgeline plot for each specified numeric variable.

  data |>
    select(...) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value, y = variable)) +
    geom_density_ridges(fill = fill, alpha = 0.7) +
    labs(
      title = "Density Plots",
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
}
