plot_scatter <- function(data, x, y) {
  # Plots a scatter plot of two numeric columns with a trend line.

  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste(deparse(substitute(y)), "vs", deparse(substitute(x))),
      x     = deparse(substitute(x)),
      y     = deparse(substitute(y))
    ) +
    theme_minimal()
}
