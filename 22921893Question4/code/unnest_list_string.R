unnest_list_string <- function(data, col) {
    # Converts a stringified list column such as ['US', 'GB'] into one row per element

    data |>
        mutate({{ col }} := str_remove_all({{ col }}, "\\[|\\]|'")) |>
        separate_longer_delim({{ col }}, delim = ", ")
}
