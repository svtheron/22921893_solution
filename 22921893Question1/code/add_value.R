add_value <- function(data, min_cost = 1) {
    # Add a measure for value-for-money metric
    # Drops rows with an extremely low cost

    data |>
        filter(cost_per_100g >= min_cost) |>
        mutate(value = rating / cost_per_100g)
}
