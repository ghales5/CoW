#' Add a data layer to the rdeck map
#'
#' This function adds a scatterplot layer to an rdeck map. The function requires
#' the id of the layer to be added and the number of the dataset to be displayed.
#' The layer can also be made visible or invisible with the `visible` argument.
#'
#' @param rdeck an rdeck object
#' @param id a character string specifying the id of the layer to be added
#' @param visible a logical value indicating whether the layer is visible or not
#' @param dataset_num an integer specifying the number of the dataset to be displayed
#' @return the rdeck object with the scatterplot layer added
#'
#'
#' @importFrom rdeck add_scatterplot_layer
#' @importFrom sf st_as_sf
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

#' Add a data centroid layer to the rdeck map
#'
#' This function adds the centroid of a dataset to an rdeck map. The function requires
#' the id of the layer to be added and the number of the dataset to be displayed.
#' The layer can also be made visible or invisible with the `visible` argument.
#'
#' @param rdeck an rdeck object
#' @param id a character string specifying the id of the layer to be added
#' @param visible a logical value indicating whether the layer is visible or not
#' @param dataset_num an integer specifying the number of the dataset to be displayed
#' @return the rdeck object with the scatterplot layer added
#'
#'
#' @importFrom rdeck add_scatterplot_layer
#' @importFrom sf st_as_sf
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
