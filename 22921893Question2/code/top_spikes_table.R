top_spikes_table <- function(data, n = 10) {
    # kable table of the top n name spikes by % change
    data |>
        slice_max(pct_change, n = n) |>
        select(name, gender, year, count, pct_change) |>
        mutate(pct_change = round(pct_change, 0)) |>
        knitr::kable(
            col.names = c("Name", "Gender", "Year", "Count", "% Change"),
            caption = "Top 10 Year-on-Year Baby Name Spikes"
        )
}
