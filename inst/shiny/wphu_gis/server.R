server <- function(input, output, session) {
  meshblock_centroids <- shp_wphu_mb %>%
    mutate(
      centroids = st_transform(centroids, crs = "+init=epsg:4326")
    )

  lga_centroids <- shp_wphu_lga |>
    mutate(
      centroids = st_transform(centroids, crs = "+init=epsg:4326")
    )

  population_centroid <- find_weighted_centroid(
    shape = shp_wphu_mb, longitude = x, latitude = y,
    person_weight = Person
  ) |>
    convert_points_to_sfc(tooltip = "Population Centre")

  shp_wphu_outline <- st_transform(
    shp_wphu_outline,
    crs = "+init=epsg:4326"
  )
  shp_wphu_poa <- st_transform(
    shp_wphu_poa,
    crs = "+init=epsg:4326"
  )

  map <- rdeck(
    map_style = mapbox_dark(),
    initial_bounds = st_bbox(shp_wphu_lga)
  ) |>
    add_polygon_layer(
      id = "population_meshblock",
      name = "Meshblock",
      filled = TRUE,
      visible = FALSE,
      data = st_transform(
        shp_wphu_mb,
        crs = "+init=epsg:4326"
      ),
      get_polygon = geometry,
      visibility_toggle = TRUE,
      get_fill_color = scale_color_category(
        col = MB_CAT21,
        palette = scales::brewer_pal("qual")
      ),
      opacity = 0.3,
      group_name = "Populations"
    ) |>
    add_scatterplot_layer(
      id = "meshblock_centroids",
      name = "Meshblock",
      data = meshblock_centroids,
      get_position = centroids,
      radius_scale = 100,
      visible = FALSE,
      group_name = "Centroids",
      visibility_toggle = TRUE
    ) |>
    add_scatterplot_layer(
      id = "lga_centroids",
      name = "LGA",
      data = lga_centroids,
      get_position = centroids,
      radius_scale = 250,
      visible = FALSE,
      group_name = "Centroids",
      visibility_toggle = TRUE
    ) |>
    add_polygon_layer(
      id = "outline_LGA",
      name = "LGA",
      filled = FALSE,
      data = st_transform(
        shp_wphu_lga,
        crs = "+init=epsg:4326"
      ),
      get_polygon = geometry,
      stroked = TRUE,
      get_line_color = "#663399ff",
      line_width_units = "pixels",
      line_width_min_pixels = 5,
      visibility_toggle = TRUE,
      group_name = "Boundaries"
    ) |>
    add_polygon_layer(
      id = "outline_meshblock",
      name = "Meshblock",
      filled = FALSE,
      visible = FALSE,
      data = st_transform(
        shp_wphu_mb,
        crs = "+init=epsg:4326"
      ),
      get_polygon = geometry,
      stroked = TRUE,
      get_line_color = "#663399ff",
      line_width_units = "pixels",
      line_width_min_pixels = 2,
      visibility_toggle = TRUE,
      # get_fill_color = scale_color_category(
      #   col = MB_CAT21,
      #   palette = scales::brewer_pal("qual")
      # ),
      # opacity = 0.3,
      group_name = "Boundaries"
    ) |>
    add_polygon_layer(
      id = "outline_postcode",
      name = "Postcode",
      filled = FALSE,
      visible = FALSE,
      data = st_transform(
        shp_wphu_poa %>% st_intersection(shp_wphu_outline),
        crs = "+init=epsg:4326"
      ),
      get_polygon = geometry,
      stroked = TRUE,
      get_line_color = "#663399ff",
      line_width_units = "pixels",
      line_width_min_pixels = 2,
      visibility_toggle = TRUE,
      group_name = "Boundaries"
    ) |>
    add_scatterplot_layer(
      id = "pop_centre",
      name = "Population Centre",
      group_name = "Centroids",
      data = population_centroid,
      get_position = geometry,
      radius_scale = 250,
      get_fill_color = "#B22222",
      visible = TRUE,
      pickable = TRUE,
      auto_highlight = TRUE,
      tooltip = tooltip
    ) |>
    add_scatterplot_layer(
      id = "data_centroid",
      name = "Data",
      group_name = "Centroids",
      data = NULL,
      get_fill_color = "#63C5DA",
      visible = FALSE,
      get_position = geometry,
      radius_scale = 250
    ) |>
    add_scatterplot_layer(
      id = "data_raw",
      name = "Data",
      data = NULL,
      get_fill_color = "#63C5DA",
      visible = FALSE,
      get_position = geometry,
      radius_scale = 200
    )
  output$map <- renderRdeck(map)

  raw_data <- reactive({
    req(input$input_latlong)
    infile <- read.csv(input$input_latlong$datapath)

    infile |>
      select(longitude, latitude, person_weight)
  })

  observe({
    req(input$input_latlong)
    rdeck_proxy("map") |>
        update_scatterplot_layer(
          id = "data_centroid",
          data = find_weighted_centroid(raw_data(), longitude, latitude, person_weight) |>
            convert_points_to_sfc(tooltip = "Data"),
          get_position = geometry,
          get_radius = 1,
          get_fill_color = "#63C5DA",
          get_line_color = "#000000ff",
          get_line_width = 1
        )
  })

  observe({
    req(input$input_latlong)
    rdeck_proxy("map") |>
        update_scatterplot_layer(
          id = "data_raw",
          data = apply(raw_data(), MARGIN=1, convert_points_to_sfc, tooltip="data") |> bind_rows(),
          get_position = geometry,
          get_radius = 1,
          get_fill_color = "#63C5DA",
          get_line_color = "#000000ff",
          get_line_width = 1,
          visible = TRUE
        )
  })
  output$files <- renderTable(raw_data())
}

# Postcode boundaries/centroids
# Uploaded centroids
# Smoothed population colours (by MB)
# Seperate centroids into geographic and population