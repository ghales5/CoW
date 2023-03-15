library(rdeck)
library(rgdal)
library(dplyr)
library(sf)
devtools::load_all()

a <- shp_wphu_mb %>%
    mutate(
        centroids = st_transform(centroids, crs="+init=epsg:4326")
    )

rdeck(
    map_style = mapbox_dark(),
    initial_bounds = st_bbox(shp_wphu_lga)
) |>
    add_polygon_layer(
        name = "LGAs",
        filled = FALSE,
        data = st_transform(
            shp_wphu_lga,
            crs = "+init=epsg:4326"),
        get_polygon = geometry,
        stroked = TRUE,
        get_line_color = "#663399ff",
        wireframe = TRUE,
        extruded = TRUE,
        get_elevation = 10,
        get_line_width = 50,
        line_width_units = "meters"
    ) |>
    add_polygon_layer(
        name = "mb_type",
        data = st_transform(
            shp_wphu_mb,
            crs = "+init=epsg:4326"
        ),
        get_polygon = geometry,
        get_fill_color = scale_color_category(
            col = MB_CAT21,
            palette = scales::brewer_pal("qual")
        ),
        opacity = 0.3
    ) |>
    add_scatterplot_layer(
        name = "meshblock_centroids",
        data = a,
        get_position = centroids,
        radius_scale = 30,
        pickable = TRUE,
        auto_highlight = TRUE
    )
    
