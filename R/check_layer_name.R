#' Check if a layer exists in an rdeck
#'
#' This function checks if a specified layer name exists in a deck in the form
#' of an rdeck object.
#' The function returns a logical value indicating whether the layer name exists
#' in the deck.
#'
#' @param rdeck an rdeck object
#' @param layer_name a character string specifying the name of the layer to check
#' @return a logical value indicating whether the layer exists in the deck
#'
#' @export
check_layer_name <- function(rdeck, layer_name) {
    layers_check <- vapply(rdeck$x$layers, function(layer) {
        layer$name == layer_name
    }, FUN.VALUE = TRUE)

    any(layers_check)
}
