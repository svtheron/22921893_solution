plot_density <- function(data, col) {
  # Plots a density curve for a numeric column.

  ggplot(data, aes(x = {{ col }})) +
    geom_density(alpha = 0.4) +
    labs(
      title = paste("Distribution of", deparse(substitute(col))),
      x     = deparse(substitute(col)),
      y     = "Density"
    ) +
    theme_minimal()
}
