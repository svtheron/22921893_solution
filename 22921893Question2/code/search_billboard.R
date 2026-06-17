search_billboard <- function(billboard, search_artist) {
    # Search artist column, return earliest chart entries first
    billboard |>
        filter(str_detect(artist, regex(search_artist, ignore_case = TRUE))) |>
        arrange(date) |>
        select(date, song, artist, rank)
}
