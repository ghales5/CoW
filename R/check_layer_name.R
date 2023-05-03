#' @export
check_layer_name <- function(rdeck, layer_name) {
    layers_check <- vapply(rdeck$x$layers, function(layer) {
        layer$name == layer_name
    }, FUN.VALUE = TRUE)

    any(layers_check)
}
