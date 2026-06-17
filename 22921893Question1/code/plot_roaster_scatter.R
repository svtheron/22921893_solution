plot_roaster_scatter <- function(data, label_top_n = 3) {
    # Scatter of cost vs rating per roaster, coloured by keyword count

    top_roasters <- data |>
        slice_max(total_keywords_mean, n = label_top_n)

    data |>
        ggplot(aes(x = cost_per_100g_mean, y = rating_mean,
                   colour = total_keywords_mean)) +
        geom_point(size = 3) +
        geom_text(
            data = top_roasters,
            aes(label = roaster),
            vjust = -1, size = 3
        ) +
        scale_colour_viridis_c() +
        labs(
            title = "Roaster comparison: cost vs rating",
            x = "Mean cost per 100g",
            y = "Mean rating",
            colour = "Mean keyword\ncount"
        ) +
        theme_minimal()
}
