default_rate_by_grade_state <- function(data, states) {
    # Facet wrapped (by state) bar plot of mean default rate

    data |>
        filter(addr_state %in% states) |>
        summarise(
            n            = n(),
            default_rate = round(100 * mean(default, na.rm = TRUE), 1),
            .by = c(grade, addr_state)
        ) |>
        ggplot(aes(x = grade, y = default_rate, fill = grade)) +
        geom_col() +
        scale_fill_brewer(palette = "Set2") +
        facet_wrap(~ addr_state) +
        labs(
            title = "Default rate by grade across selected states",
            x     = "Grade",
            y     = "Default rate (%)"
        ) +
        theme_minimal()
}
