billboard_weeks_plot <- function(billboard, search_artist, year_range) {
    # Bar plot of weeks charted per year, for one artist
    billboard |>
        filter(str_detect(artist, regex(search_artist, ignore_case = TRUE))) |>
        mutate(year = year(date)) |>
        summarise(weeks = n(), .by = year) |>
        ggplot(aes(x = year, y = weeks)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        xlim(year_range) +
        labs(
            title = paste(search_artist, "- weeks on Billboard chart"),
            x = "Year",
            y = "Weeks"
        )
}
