save_plot <- function(plot, filename, width = 8, height = 5) {
    # This saves a ggplot object to the figures folder as png
    # It assumes the figures folder already exists
    ggsave(
        filename = paste0("figures/", filename),
        plot     = plot,
        width    = width,
        height   = height,
        device   = "png"
    )
}
