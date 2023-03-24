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
    ## MESHBLOCK TYPES
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
    ## MESHBLOCK CENTROIDS
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
    ## LGA CENTROIDS
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
    ## LGA OUTLINES
    add_boundary_layer(
      id = "outline_LGA",
      name = "LGA",
      data = shp_wphu_lga,
      tooltip = LGA_NAME22
    ) |>
    ## MESHBLOCK OUTLINES
    add_boundary_layer(
      id = "outline_meshblock",
      name = "Meshblock",
      data = shp_wphu_mb,
      visible = FALSE
    ) |>
    ## POSTCODE OUTLINES
    add_boundary_layer(
      id = "outline_postcode",
      name = "Postcode",
      data = shp_wphu_poa %>% st_intersection(shp_wphu_outline),
      tooltip = POA_NAME21,
      visible = FALSE
    ) |>
    ## ALL OF WPHU CENTROID
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
    ## DATA CENTROID
    add_data_centroid_layer(
      id = "data_centroid_1",
      dataset_num = 1
    ) |>
    ## RAW DATA
    add_data_layer(
      id = "data_1",
      dataset_num = 1
    )
  output$map <- renderRdeck(map)

  # Number of data file upload boxes
  num_data_files <- reactiveVal(1)

  # Loading in of data
  # Is a list of all of the uploaded files.
  raw_data <- reactive({
    lapply(seq(1, num_data_files()), function(i) {
      fname <- paste0("data_", i)
      if (!is.null(input[[fname]])) {
        infile <- read.csv(input[[fname]]$datapath)
        return(infile |>
          select(longitude, latitude, person_weight))
      } else {
        return(NULL)
      }
    })
  })


  # Update's the data_i scatterplot layer
  # When new data is uploaded (i.e. num_data_files changes or
  # raw_data changes)
  observe({
    lapply(seq_along(raw_data()), function(i) {
      data_name <- paste0("data_", i)
      if (!is.null(input[[data_name]])) {
        rdeck_proxy("map") |>
          update_scatterplot_layer(
            id = data_name,
            data = apply(raw_data()[[i]], MARGIN = 1, convert_points_to_sfc, tooltip = "data") |> bind_rows(),
            get_position = geometry,
            get_radius = 1,
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1,
            visible = TRUE
          )
      }
    })
  })

  # Updates the centroids
  # Uses identical logic to drawing the actual data
  # But the data call finds the weighted centroid.
  observe({
    lapply(seq_along(raw_data()), function(i) {
      data_name <- paste0("data_", i)
      if (!is.null(input[[data_name]])) {
        rdeck_proxy("map") |>
          update_scatterplot_layer(
            id = paste0("data_centroid_", i),
            data = find_weighted_centroid(raw_data()[[i]], longitude, latitude, person_weight) |>
              convert_points_to_sfc(tooltip = "Data"),
            get_position = geometry,
            get_radius = 1,
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1
          )
      }
    })
  })


  observe({
    lapply(seq_along(raw_data()), function(i) {
      data_name <- paste0("data_", i)
      req(input[[data_name]], cancelOutput = TRUE)
      rdeck_proxy("map") |>
        update_scatterplot_layer(
          id = data_name,
          get_fill_color = input[[paste0("data_colour_", i)]]
        ) |>
        update_scatterplot_layer(
          id = paste0("data_centroid_", i),
          get_fill_color = input[[paste0("data_colour_", i)]]
        )
    })
  })



  output$fileInputs <- renderUI({
    lapply(1:num_data_files(), function(i) {
      wellPanel(
        fileInput(
          paste0("data_", i), "Data upload (CSV)",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        colourInput(
          paste0("data_colour_", i),
          label = "Data Colour",
          showColour = "background"
        )
      )
    }) |> tagList()
  })


  observeEvent(
    input$dataIncrease,
    {
      num_data_files(num_data_files() + 1)

      layer_name <- paste0("data_", num_data_files())
      centroid_layer_name <- paste0("data_centroid_", num_data_files())
      if (!check_layer_name(rdeck_proxy("map"), layer_name)) {
        rdeck_proxy("map") |>
          add_data_layer(
            id = layer_name,
            dataset_num = num_data_files()
          ) |>
          add_data_centroid_layer(
            id = centroid_layer_name,
            dataset_num = num_data_files()
          )
      }
    }
  )
  observeEvent(
    input$dataDecrease,
    {
      if (num_data_files() >= 2) {
        num_data_files(num_data_files() - 1)
      }
    }
  )
}

# Postcode boundaries/centroids
# Uploaded centroids
# Smoothed population colours (by MB)
# Seperate centroids into geographic and population
# Multiple files uploaded
# Geographic centroid for entire reason
# Add hover labels for postcode/LGA etc
# Remove airports from mapbox
