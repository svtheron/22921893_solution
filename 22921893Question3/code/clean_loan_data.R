clean_loan_data <- function(data) {
    # Selects relevant columns, creates a binary default outcome from loan_status
    # This function excludes unresolved loans
    # It also makes the emp_length column into a properly formatted numeric
    # Filters dtis values

    data |>
        select(
            loan_status, grade, sub_grade, home_ownership, emp_length,
            dti, annual_inc, int_rate, term, addr_state, purpose
        ) |>
        filter(loan_status %in% c("Fully Paid", "Charged Off", "Default")) |>
        mutate(
            default    = if_else(loan_status == "Fully Paid", 0, 1),
            emp_length = as.numeric(str_extract(emp_length, "\\d+"))
        ) |>
        filter((dti >= 0 & dti != 999) | is.na(dti))
}
