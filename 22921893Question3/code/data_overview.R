data_overview <- function(data) {
    # Gives a brief overview of a dataset/frame
    # includes: type, missing count/%, and unique value count.

    tibble(
        variable    = names(data),
        type        = map_chr(data, ~ class(.x)[1]),
        n_missing   = map_int(data, ~ sum(is.na(.x))),
        pct_missing = round(100 * n_missing / nrow(data), 1),
        n_unique    = map_int(data, ~ n_distinct(.x))
    )
}
