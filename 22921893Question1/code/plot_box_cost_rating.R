plot_box_cost_rating <- function(data) {
    # Boxplot of cost by rating

    data |>
        ggplot(
            aes(x = cost_per_100g, y = factor(rating))) +
        geom_boxplot() +
        scale_x_log10() +
        theme_minimal() +
        labs(
            title = "Coffee cost by rating",
            x     = "Cost per 100g (USD, log scale)",
            y     = "Rating"
        )
}
