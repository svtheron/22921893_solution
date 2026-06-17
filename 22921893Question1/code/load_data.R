load_data <- function(file_path) {
    # This function reads a CSV and converts the column names to snake_case

    data <- read_csv(file_path) |>
        janitor::clean_names()

    data
}
