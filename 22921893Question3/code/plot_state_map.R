plot_state_map <- function(data) {
    # This function uses the usmap package
    # it aggregate data & renames column for usmap compatibility

    map_data <- data |>
        group_by(addr_state) |>
        summarise(default_rate = mean(default, na.rm = TRUE), .groups = "drop") |>
        rename(state = addr_state)

    # Plot
    plot_usmap(data = map_data, values = "default_rate", color = "white") +
        scale_fill_continuous(
            low = "lightblue",
            high = "darkred",
            name = "Default Rate",
            labels = scales::percent
        ) +
        labs(title = "Geographic Distribution of Loan Defaults") +
        theme(legend.position = "right")
}
