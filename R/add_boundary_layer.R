add_boundary_layer <- function(rdeck, id, name, data, ...) {
    add_polygon_layer(
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
