genre_runtime_violin_plot <- function(data, min_n = 30) {
    # Violin plot of runtime distribution per genre

    keep_genres <- data |>
        filter(genres != "") |>
        summarise(n_movies = n(), .by = genres) |>
        filter(n_movies >= min_n) |>
        pull(genres)

    data |>
        filter(genres %in% keep_genres) |>
        drop_na(runtime) |>
        ggplot(aes(x = fct_reorder(genres, runtime), y = runtime)) +
        geom_violin(fill = "tomato", alpha = 0.6) +
        coord_flip() +
        theme_minimal() +
        labs(
            title = "Movie Runtime Distribution by Genre",
            x     = "Genre",
            y     = "Runtime (minutes)"
        )
}
