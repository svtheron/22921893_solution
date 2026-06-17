top_n_names <- function(data, n = 25) {
    # Filters to the top n ranked names per year-gender pairing
    data |>
        filter(rank <= n)
}
