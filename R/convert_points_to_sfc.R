convert_points_to_sfc <- function(centroid_points = ., longitude_col = "longitude", latitude_col = "latitude", tooltip) {
    long <- as.numeric(centroid_points[longitude_col])
    lat <- as.numeric(centroid_points[latitude_col])
    point <- st_point(c(long, lat)) |> st_sfc(crs = "+init=epsg:4326")

    converted_frame <- st_sf(data.frame(
        a = 1, geom = point
    )) |>
        mutate(tooltip = tooltip)
}
