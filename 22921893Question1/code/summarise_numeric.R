summarise_numeric <- function(data, group_var) {
    # Computes mean and median for rating, cost_per_100g, and total_keywords,
    # grouped by the chosen categorical variable

    data |>
        summarise(
            across(c(rating, cost_per_100g, total_keywords), list(
                mean   = mean,
                median = median
            )),
            .by = {{ group_var }}
        )
}

