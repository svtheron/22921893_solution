plot_cost_rating_value <- function(data) {
    # Scatter plot of cost vs rating for a set of roasters

    data |>
        ggplot(aes(x = median_cost, y = median_rating, colour = median_value)) +
        geom_point(size = 3) +
        geom_text(aes(label = roaster), vjust = -0.8, size = 3) +
        scale_colour_viridis_c() +
        labs(
            title  = "Cost vs rating for top 10 roasters by value",
            x      = "Median cost per 100g (USD)",
            y      = "Median rating",
            colour = "Median value"
        ) +
        theme_minimal()
}
