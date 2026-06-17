genre_by_country_plot <- function(data) {
    # Faceted bar chart of top genres per country

    data |>
        ggplot(aes(x = fct_reorder(genres, n), y = n, fill = production_countries)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        facet_wrap(~production_countries, scales = "free_y") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal() +
        labs(
            title = "Top 5 Movie Genres by Country",
            x     = "Genre",
            y     = "Number of Movies"
        )
}

