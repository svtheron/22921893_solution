
plot_country_density <- function(data, variable, countries) {
    # This function creates a Density plot to compare numeric variable across chosen countries

    data |>
        filter(loc_country %in% countries) |>
        ggplot(aes(x = {{ variable }}, fill = loc_country)) +
        scale_x_log10(labels = scales::comma) +
        geom_density(alpha = 0.4) +
        theme_minimal()
}
