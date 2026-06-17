top_words_by_country <- function(data, countries, top_n = 10) {
    # This function tokenizes descriptions into words and counts top N words (per country)

    data |>
        filter(production_countries %in% countries) |>
        unnest_tokens(word, description) |>
        anti_join(stop_words, by = "word") |>
        count(production_countries, word, sort = TRUE) |>
        slice_max(n, n = top_n, by = production_countries)
}
