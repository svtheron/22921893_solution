rank_persistence_plot <- function(data) {
    # Time-series plot of rank correlatios
    data |>
        ggplot(
            aes(x = base_year, y = correlation, colour = factor(lag))
            ) +
        geom_line() +
        facet_wrap(~ gender) +
        scale_colour_brewer(palette = "Set2", name = "Lag (years)") +
        labs(
            title = "Persistence of Top-25 Baby Names",
            x = "Year",
            y = "Spearman rank correlation"
        ) +
        theme_minimal()
}
