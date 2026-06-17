name_spike_plot <- function(data, search_name, search_gender, year_range) {
    # Line plot of one name's count over time
    data |>
        filter(name == search_name, gender == search_gender) |>
        ggplot(aes(x = year, y = count)) +
        geom_line(colour = "steelblue") +
        xlim(year_range) +
        theme_minimal() +
        labs(
            title = paste(search_name, "- naming trend over time"),
            x = "Year",
            y = "Count"
        )
}
