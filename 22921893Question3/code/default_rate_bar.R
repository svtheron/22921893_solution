default_rate_bar <- function(data, group_var) {
    # Calculates default rate by group, then plots as a bar chart

    data |>
        summarise(
            n            = n(),
            default_rate = round(100 * mean(default, na.rm = TRUE), 1),
            .by = {{ group_var }}
        ) |>
        mutate(group_var = fct_reorder({{ group_var }}, default_rate)) |>
        ggplot(aes(x = group_var, y = default_rate, fill = group_var)) +
        geom_col() +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = "Default rate by group",
            x     = NULL,
            y     = "Default rate (%)"
        ) +
        theme_minimal()
}
