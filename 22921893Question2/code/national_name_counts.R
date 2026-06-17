national_name_counts <- function(data) {
    # Sum counts across states
    # also converts names to lowercase
    data |>
        summarise(
            count = sum(Count, na.rm = TRUE),
            .by = c(Name, Year, Gender)
        ) |>
        janitor::clean_names()
}

