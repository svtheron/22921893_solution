correlation_period_table <- function(data, cutoff_year = 1990) {
    # Compare average rank correlation before vs after 1990
    data |>
        mutate(
            period = if_else(base_year < cutoff_year, "Pre 1990", "Post 1990")
        ) |>
        summarise(
            mean_correlation = mean(correlation, na.rm = TRUE),
            .by = c(period, lag)
        ) |>
        arrange(lag, period) |>
        knitr::kable(
            col.names = c("Period", "Lag (years)", "Mean Correlation"),
            caption = "Mean Correlation around 1990",
            digits = 3
        )
}
