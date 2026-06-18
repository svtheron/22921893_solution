numeric_summary <- function(data) {
    # Summary statistics for numeric column

    num_data <- data |>
        select(where(is.numeric))

    tibble(
        variable = names(num_data),
        mean     = map_dbl(num_data, ~ mean(.x, na.rm = TRUE)),
        median   = map_dbl(num_data, ~ median(.x, na.rm = TRUE)),
        sd       = map_dbl(num_data, ~ sd(.x, na.rm = TRUE)),
        min      = map_dbl(num_data, ~ min(.x, na.rm = TRUE)),
        max      = map_dbl(num_data, ~ max(.x, na.rm = TRUE))
    ) |>
        mutate(across(where(is.numeric), ~ round(.x, 1)))
}
