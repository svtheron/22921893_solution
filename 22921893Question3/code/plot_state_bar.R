plot_state_bar <- function(data, min_loans = 2000) {
    # Groups and filters aggregate state data , then creates a barplot
    state_data <- data |>
        group_by(addr_state) |>
        summarise(
            default_rate = mean(default, na.rm = TRUE),
            total_loans = n(),
            .groups = "drop"
        ) |>
        filter(total_loans >= min_loans)

    # Plot
    ggplot(state_data, aes(x = reorder(addr_state, default_rate), y = default_rate)) +
        geom_col(fill = "tomato") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(
            title = "Loan Default Rate by State",
            x = "State",
            y = "Default Rate"
        ) +
        theme_minimal()
}
