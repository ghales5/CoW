add_data_layer <- function(rdeck, id, ...) {
    add_scatterplot_layer(
        rdeck,
        id = id,
        name = "Data",
        data = NULL,
        get_fill_color = "#63C5DA",
        visible = FALSE,
        get_position = geometry,
        radius_scale = 200
    )
}
