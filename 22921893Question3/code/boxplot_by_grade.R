boxplot_by_grade <- function(data, y_var, y_label) {
    # Boxplot of a numeric variable across credit grades

    data |>
        ggplot(aes(x = grade, y = {{ y_var }}, fill = grade)) +
        geom_boxplot() +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = "Distribution by credit grade",
            x     = "Grade",
            y     = y_label
        ) +
        theme_minimal()
}
