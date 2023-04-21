#' @export
add_data_layer <- function(rdeck, id, visible = FALSE, dataset_num, ...) {
    rdeck::add_scatterplot_layer(
        rdeck,
        id = id,
        name = paste0("Dataset ", dataset_num),
        data = NULL,
        get_fill_color = "#63C5DA",
        visible = visible,
        get_position = geometry,
        radius_scale = 5,
        group_name = "Data",
        radius_min_pixels = 5,
        opacity = 0.2
    )
}

#' @export
add_data_centroid_layer <- function(rdeck, id, visible = FALSE, dataset_num, ...) {
    rdeck::add_scatterplot_layer(
        rdeck,
        id = id,
        name = paste0("Dataset ", dataset_num),
        data = NULL,
        get_fill_color = "#63C5DA",
        visible = visible,
        get_position = geometry,
        radius_scale = 250,
        group_name = "Population Centroids"
    )
}
