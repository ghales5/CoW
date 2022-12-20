
#' Find the weighted centroid of a given shapefile with provided weights
#'
#' Finds the weighted centroid of a given set of coordinates, weighted by a
#' user-specified column
#'
#' @param shape Object of class `sf` that has the coordinates and weights as features.
#' @param longitude Column containing the longitude of the centroids to find the
#' centre of.
#' @param latitude Column containing the latitude of the centroids to find the
#' centre of.
#' @param person_weight Column containing the relative weights for each centroid.
#'
#' @return A tibble with the following columns:
#' * `centre_longitude`: Longitude of the geographic centre
#' * `centre_latitude`: Latitude of the geographic centre
#' @examples
#' find_weighted_centroid(wphu_mb_shp, x, y, Person)
find_weighted_centroid <- function(shape, longitude, latitude, person_weight) {
    shape %>%
        dplyr::select(
            {{ longitude }},
            {{ latitude }},
            {{ person_weight }}
        ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
            weighted_x = {{ longitude }} * {{ person_weight }},
            weighted_y = {{ latitude }} * {{ person_weight }}
        ) %>%
        dplyr::summarise(
            sXw = sum(weighted_x),
            sYw = sum(weighted_y),
            sW = sum({{ person_weight }}),
            centre_longitude = sXw / sW,
            centre_latitude = sYw / sW
        ) %>%
        dplyr::select(
            -sXw, -sYw, -sW
        )
}
