count_keywords <- function(data, keywords) {
    # This function counts how many times each keyword appears per coffee
    # It counts accross all three description columns
    # Then, it adds a count column per keyword.

    data <- data |>
        mutate(
            combined_desc = str_to_lower(paste(desc_1, desc_2, desc_3)) # generating a new column with the combined descriptions
            )

    count_one_keyword <- function(word) {
        str_count(data$combined_desc, word) # counting each word
    }

    keyword_counts <- keywords |>
        map(count_one_keyword) |> # applying the count function to each column
        set_names(keywords) |>
        as_tibble()

    keyword_counts$total_keywords <- rowSums(keyword_counts) # creating a column for total keyword appearance

    bind_cols(data, keyword_counts) # creating a new tibble
}
