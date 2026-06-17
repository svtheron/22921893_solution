top_words_plot <- function(data) {
    # Faceted bar chart of top words per country

    data |>
        ggplot(aes(x = reorder_within(word, n, production_countries), y = n,
                   fill = production_countries)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_x_reordered() +
        facet_wrap(~production_countries, scales = "free_y") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal() +
        labs(
            title = "Top 10 Description Words by Country",
            x     = "Word",
            y     = "Frequency"
        )
}
