rank_correlation <- function(top25, full_ranked, lag) {
    # For each base year's top 25 names, look up their rank "lag" years later
    # Then, compute the Spearman correlation between the base and future ranks

    base <- top25 |>
        select(name, gender, base_year = year, base_rank = rank)

    future <- full_ranked |>
        select(name, gender, future_year = year, future_rank = rank)

    comparison <- base |>
        mutate(
            future_year = base_year + lag
            ) |>
        inner_join(future, by = c("name", "gender", "future_year"))

    comparison |>
        summarise(
            correlation = cor(base_rank, future_rank, method = "spearman"),
            .by = c(base_year, gender)
        ) |>
        mutate(lag = lag)
}
