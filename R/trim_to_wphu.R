#' Trim points to within WPHU (Western Public Health Unit)
#'
#' This function takes a set of points and determines which points fall within
#' WPHU (Western Public Health Unit) based on their longitude and latitude
#' coordinates. The function uses the 'sf' package to create points and check
#' if they intersect with the outline of the WPHU shapefile provided to the
#' function. If a point is within the WPHU, it is kept in the final output.
#' If a point is outside of the WPHU, it is discarded from the final output.
#'
#' @param pts a data frame of points containing columns 'longitude' and 'latitude'
#' @return a data frame of points that are within WPHU
#'
#' @examples
#'
#' data(example_lga_person_weights)
#'
#' trim_to_wphu(example_lga_person_weights)
#'
#' @importFrom rdeck sfc_point
#' @importFrom sf st_intersects
#' @importFrom purrr map2
#' @export
trim_to_wphu <- function(pts) {
    inside_wphu <- purrr::map2(
        .x = pts$longitude,
        .y = pts$latitude,
        .f = function(long, lat) {
            sfc_point(long, lat) |>
                st_intersects(shp_wphu_outline) |>
                lengths()
        }
    ) |> unlist()

    pts[pts[inside_wphu] > 0]
}
