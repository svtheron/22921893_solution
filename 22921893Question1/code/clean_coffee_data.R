clean_coffee_data <- function(data) {
    # Reformats data from Mon-YY format, and
    # Fixes a possible typo in the loc_country column
    # Drops na's in the roast column
    data |>
        mutate(
            review_date = lubridate::my(review_date),
            loc_country = if_else(
                loc_country == "United States And Floyd",
                "United States", loc_country
            ),
            roaster = roaster |>
                stri_trans_general("Latin-ASCII") |>
                gsub(pattern = "[’‘]", replacement = "'"),
            roaster = if_else(
                str_detect(roaster, "Simon Hsieh"),
                "Simon Hsieh Aroma Roast Coffees", roaster
            )
        ) |>
        drop_na(roast)
}
