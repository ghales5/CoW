#' Add a boundary layer to a deck
#'
#' This function adds a boundary layer to a deck in the form of a R deck object.
#' The function takes an id, name, and spatial data, and creates a polygon layer that can be added to the deck.
#' Uses the 'sf' package to transform the spatial data to WGS84 CRS and extract the polygon geometry.
#' Additional arguments can be passed to modify the layer appearance or behavior.
#'
#' @param rdeck an rdeck object
#' @param id a character string specifying the id of the layer to add
#' @param name a character string specifying the name of the layer to add
#' @param data a spatial object containing the boundary data to add
#' @param ... additional arguments to modify the layer appearance or behavior, passed to `add_polygon_layer`.
#' @return a deck object with the new boundary layer added
#'
#' @importFrom rdeck add_polygon_layer
#' @importFrom sf st_transform st_union st_geometry geometry
#' @export
add_boundary_layer <- function(rdeck, id, name, data, ...) {
    rdeck::add_polygon_layer(
        rdeck,
        id = id,
        name = name,
        data = st_transform(
            data,
            crs = "+init=epsg:4326"
        ),
        get_polygon = geometry,
        stroked = TRUE,
        get_line_color = "#663399ff",
        line_width_units = "pixels",
        line_width_min_pixels = 5,
        visibility_toggle = TRUE,
        group_name = "Boundaries",
        pickable = TRUE,
        get_fill_color = "#66339900",
        ...
    )
}
