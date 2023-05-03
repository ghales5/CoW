#' Convert a data frame of points to an sf object
#'
#' This function takes a data frame of points and converts it to an sf object.
#' The function requires the longitude and latitude columns to be specified.
#' A tooltip can also be added to the converted sf object.
#'
#' @param centroid_points a data frame containing the points to be converted
#' @param longitude_col a character string specifying the name of the longitude column in centroid_points
#' @param latitude_col a character string specifying the name of the latitude column in centroid_points
#' @param tooltip a character string specifying the tooltip to be added to the converted sf object
#' @return an sf object containing the converted points and the specified tooltip
#'
#' @importFrom sf st_point st_sfc st_sf
#' @importFrom dplyr mutate
#' @export
convert_points_to_sfc <- function(centroid_points = ., longitude_col = "longitude", latitude_col = "latitude", tooltip) {
    long <- as.numeric(centroid_points[longitude_col])
    lat <- as.numeric(centroid_points[latitude_col])
    point <- st_point(c(long, lat)) |> st_sfc(crs = "+init=epsg:4326")

    converted_frame <- st_sf(data.frame(
        a = 1, geom = point
    )) |>
        dplyr::mutate(tooltip = tooltip)
}
