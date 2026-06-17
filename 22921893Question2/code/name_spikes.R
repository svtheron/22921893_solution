name_spikes <- function(data, min_count = 500) {
    # Year-on-year % change in count, per name+gender
    data |>
        arrange(year) |>
        mutate(
            pct_change = (count - lag(count)) / lag(count) * 100,
            .by = c(name, gender)
        ) |>
        filter(count >= min_count, !is.na(pct_change)) |>
        arrange(desc(pct_change))
}
