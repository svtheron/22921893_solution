default_rate_by_group <- function(data, group_var) {
    # Calculates default rate and sample size for any grouping column
    data |>
        summarise(
            n            = n(),
            default_rate = round(100 * mean(default, na.rm = TRUE), 1),
            .by = {{ group_var }}
        )
}
