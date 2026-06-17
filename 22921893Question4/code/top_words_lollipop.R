top_words_lollipop <- function(data, country, top_n = 10) {
    # Lollipop chart of top words for a single country
    # Used to get lollipop chart for south africa

    data |>
        filter(production_countries == country) |>
        ggplot(aes(x = fct_reorder(word, n), y = n)) +
        geom_segment(aes(xend = word, y = 0, yend = n)) +
        geom_point(size = 3, colour = "tomato") +
        coord_flip() +
        theme_minimal() +
        labs(
            title = "Top Description Words - South Africa",
            x     = "Word",
            y     = "Frequency"
        )
}
