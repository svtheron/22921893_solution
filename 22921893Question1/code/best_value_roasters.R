best_value_roasters <- function(data, min_n = 5) {
    # Rank roasters by median value (to account for outliers)
    # Additonally, restricts to roasters with more than 5 coffees

    data |>
        summarise(
            n             = n(),
            median_cost   = median(cost_per_100g, na.rm = TRUE),
            median_rating = median(rating, na.rm = TRUE),
            median_value  = median(value, na.rm = TRUE),
            .by = roaster
        ) |>
        filter(n >= min_n) |>
        arrange(desc(median_value))
}
