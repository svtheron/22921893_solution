rank_names <- function(data) {
    # Ranks names by count within year-gender group
    data |>
        mutate(
            rank = rank(-count),
            .by = c(year, gender)
        )
}
