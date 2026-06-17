plot_cor_heatmap <- function(data, ...) {
    # This function creates a correlation heatmap for the chosen numeric variables

    cor_data <- data |>
        select(...) |>
        cor()

    cor_data |>
        as_tibble(rownames = "var1") |>
        pivot_longer(-var1, names_to = "var2", values_to = "correlation") |>
        ggplot(
            aes(x = var1, y = var2, fill = correlation)
            ) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
        theme_minimal() +
        labs(
            title = "Correlation Heatmap",
             x = "",
             y = "")
}
