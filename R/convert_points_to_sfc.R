convert_points_to_sfc <- function(centroid_points = ., tooltip) {
    centroid_points <- centroid_points |>
        as.numeric() |>
        st_point() |>
        st_sfc(crs = "+init=epsg:4326")
    centroid_points <- st_sf(data.frame(a = 1, geom = centroid_points)) |>
        mutate(tooltip = tooltip)
}
