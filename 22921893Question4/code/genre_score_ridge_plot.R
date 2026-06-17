genre_score_ridge_plot <- function(data, min_n = 30) {
    # Ridge plot of IMDb score distribution per genre

    keep_genres <- data |>
        filter(genres != "") |>
        summarise(n_movies = n(), .by = genres) |>
        filter(n_movies >= min_n) |>
        pull(genres)

    data |>
        filter(genres %in% keep_genres) |>
        drop_na(imdb_score) |>
        ggplot(aes(x = imdb_score, y = fct_reorder(genres, imdb_score), fill = after_stat(x))) +
        geom_density_ridges_gradient() +
        scale_fill_viridis_c(option = "inferno") +
        theme_minimal() +
        labs(
            title = "IMDb Score Distribution by Genre",
            x     = "IMDb Score",
            y     = "Genre"
        )
}
