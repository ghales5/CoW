server <- function(input, output, session) {
  meshblock_centroids <- CoW::shp_wphu_mb %>%
    mutate(
      centroids = st_transform(centroids, crs = "+init=epsg:4326")
    )

  lga_centroids <- CoW::shp_wphu_lga |>
    mutate(
      centroids = st_transform(centroids, crs = "+init=epsg:4326")
    )

  poa_centroids <- CoW::shp_wphu_poa |>
    mutate(
      centroids = st_transform(centroids, crs = "+init=epsg:4326")
    ) |>
    st_transform(crs = "+init=epsg:4326")


  population_centroid <- CoW::find_weighted_centroid(
    shape = CoW::shp_wphu_mb, longitude = x, latitude = y,
    person_weight = Person
  ) |>
    CoW::convert_points_to_sfc(longitude_col = "centre_longitude", latitude_col = "centre_latitude", tooltip = "Population Centre")

  shp_wphu_outline <- st_transform(
    CoW::shp_wphu_outline,
    crs = "+init=epsg:4326"
  )
  shp_wphu_poa <- st_transform(
    CoW::shp_wphu_poa,
    crs = "+init=epsg:4326"
  )

  population_grid <- st_make_grid(
    meshblock_centroids,
    cellsize = c(0.005, 0.005),
    square = FALSE
  ) |>
    st_transform(crs = "+init=epsg:4326") |>
    st_as_sf() %>%
    mutate(pt_count = lengths(st_intersects(., meshblock_centroids$centroids))) %>%
    filter(pt_count > 0)

  entire_region_centroid <- CoW::shp_wphu_lga |>
    st_union() |>
    st_centroid() |>
    st_coordinates() |>
    as.data.frame() |>
    CoW::convert_points_to_sfc(longitude_col = "X", latitude_col = "Y", tooltip = "Geographic Centre")

  # Dummy data frame for the legend - just stores the centroids because
  # there's less data to carry around.
  # We have visibility_toggle = FALSE for this layer
  # so it can never be disabled.
  combined_data_for_legend <- reactive({
    rbind(
      entire_region_centroid |> mutate(color = "#9bd318"),
      population_centroid |> mutate(color = "#B22222"),
      lapply(seq_along(raw_data()), function(i) {
        data_name <- paste0("data_", i)
        req(input[[data_name]], cancelOutput = TRUE)
        CoW::find_weighted_centroid(raw_data()[[i]], longitude, latitude, person_weight) |>
          CoW::convert_points_to_sfc(tooltip = tools::file_path_sans_ext(unique(raw_data()[[i]]$filename)), longitude_col = "centre_longitude", latitude_col = "centre_latitude") |>
          mutate(color = input[[paste0("data_colour_", i)]])
      }) |> bind_rows()
    )
  })

  env <- environment()
  env$map <- rdeck(
    map_style = mapbox_dark(),
    initial_bounds = st_bbox(CoW::shp_wphu_lga)
  ) |>
    ## MESHBLOCK TYPES
    rdeck::add_polygon_layer(
      id = "population_meshblock",
      name = "Meshblock Types",
      filled = TRUE,
      visible = FALSE,
      data = st_transform(
        CoW::shp_wphu_mb,
        crs = "+init=epsg:4326"
      ),
      get_polygon = geometry,
      visibility_toggle = TRUE,
      get_fill_color = rdeck::scale_color_category(
        col = MB_CAT21,
        palette = scales::brewer_pal("qual")
      ),
      opacity = 0.3
    ) |>
    rdeck::add_polygon_layer(
      id = "meshblock_pop",
      name = "Population",
      data = population_grid,
      get_fill_color = rdeck::scale_color_power(
        col = pt_count,
        legend = FALSE
      ),
      visible = FALSE,
      get_polygon = x,
      visibility_toggle = TRUE,
      opacity = 0.3
    ) |>
    ## MESHBLOCK CENTROIDS
    rdeck::add_scatterplot_layer(
      id = "meshblock_centroids",
      name = "Meshblock",
      data = meshblock_centroids,
      get_position = centroids,
      radius_scale = 100,
      visible = FALSE,
      group_name = "Geographic Centroids",
      visibility_toggle = TRUE
    ) |>
    ## POA CENTROIDS
    rdeck::add_scatterplot_layer(
      id = "poa_centroids",
      name = "Postcode",
      data = poa_centroids,
      get_position = centroids,
      radius_scale = 250,
      visible = FALSE,
      group_name = "Geographic Centroids",
      visibility_toggle = TRUE,
      get_fill_color = "#87CEEB"
    ) |>
    ## LGA CENTROIDS
    rdeck::add_scatterplot_layer(
      id = "lga_centroids",
      name = "LGA",
      data = lga_centroids,
      get_position = centroids,
      radius_scale = 250,
      visible = FALSE,
      group_name = "Geographic Centroids",
      visibility_toggle = TRUE
    ) |>
    ## LGA OUTLINES
    CoW::add_boundary_layer(
      id = "outline_LGA",
      name = "LGA",
      data = CoW::shp_wphu_lga,
      tooltip = LGA_NAME22
    ) |>
    ## MESHBLOCK OUTLINES
    CoW::add_boundary_layer(
      id = "outline_meshblock",
      name = "Meshblock",
      data = CoW::shp_wphu_mb,
      visible = FALSE
    ) |>
    ## POSTCODE OUTLINES
    CoW::add_boundary_layer(
      id = "outline_postcode",
      name = "Postcode",
      data = shp_wphu_poa,
      tooltip = POA_NAME21,
      visible = FALSE
    ) |>
    ## ALL OF WPHU CENTROID
    rdeck::add_scatterplot_layer(
      id = "pop_centre",
      name = "WPHU",
      group_name = "Population Centroids",
      data = population_centroid,
      get_position = geometry,
      radius_scale = 250,
      get_fill_color = "#B22222",
      visible = TRUE,
      pickable = TRUE,
      auto_highlight = TRUE,
      tooltip = tooltip
    ) |>
    ## ALL OF WPHU GEOGRAPHIC CENTROID
    rdeck::add_scatterplot_layer(
      id = "geo_centre",
      name = "WPHU",
      group_name = "Geographic Centroids",
      data = entire_region_centroid,
      get_position = geometry,
      radius_scale = 250,
      get_fill_color = "#9bd318",
      visible = TRUE,
      pickable = TRUE,
      auto_highlight = TRUE
    ) |>
    ## DATA CENTROID
    CoW::add_data_centroid_layer(
      id = "data_centroid_1",
      dataset_num = 1
    ) |>
    ## RAW DATA
    CoW::add_data_layer(
      id = "data_1",
      dataset_num = 1
    )
  output$map <- renderRdeck(env$map)

  # Number of data file upload boxes
  num_data_files <- reactiveVal(1)

  # Loading in of data
  # Is a list of all of the uploaded files.
  raw_data <- reactive({
    lapply(seq(1, num_data_files()), function(i) {
      fname <- paste0("data_", i)
      if (!is.null(input[[fname]])) {
        infile <- read.csv(input[[fname]]$datapath)
        # Check if the input file is a template with specific columns:
        if ("postcode_or_lga" %in% colnames(infile) & "number_of_people" %in% colnames(infile)) {
          infile <- infile |>
            left_join(CoW::poa_lga_centroids, by = "postcode_or_lga") |>
            select(-postcode_or_lga) |>
            rename(person_weight = number_of_people)
        }
        return(infile |>
          dplyr::select(longitude, latitude, person_weight) |>
          dplyr::mutate(filename = input[[fname]]$name))
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
        rdeck::rdeck_proxy("map") |>
          rdeck::update_scatterplot_layer(
            id = data_name,
            data = apply(raw_data()[[i]], MARGIN = 1, CoW::convert_points_to_sfc, tooltip = "data") |>
              dplyr::bind_rows() |>
              dplyr::mutate(person_weight = raw_data()[[i]]$person_weight),
            get_position = geometry,
            get_radius = person_weight,
            radius_max_pixels = 20,
            radius_units = "pixels",
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1,
            visible = TRUE,
            name = tools::file_path_sans_ext(unique(raw_data()[[i]]$filename))
          )

        env$map <- env$map |>
          rdeck::update_scatterplot_layer(
            id = data_name,
            data = apply(raw_data()[[i]], MARGIN = 1, CoW::convert_points_to_sfc, tooltip = "data") |>
              dplyr::bind_rows() |>
              dplyr::mutate(person_weight = raw_data()[[i]]$person_weight),
            get_position = geometry,
            get_radius = person_weight,
            radius_max_pixels = 20,
            radius_units = "pixels",
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1,
            visible = TRUE,
            name = tools::file_path_sans_ext(unique(raw_data()[[i]]$filename))
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
        centroid <- CoW::find_weighted_centroid(raw_data()[[i]], longitude, latitude, person_weight) |>
          CoW::convert_points_to_sfc(tooltip = "Data", longitude_col = "centre_longitude", latitude_col = "centre_latitude")

        # st_intersects will return an empty list if the intersection is empty
        # We can check with length*s* (not length) to see if this is the case
        # If it is, lauch a dialog warning box saying it's outside the boundary
        # But allow computation and updating to continue.
        if (lengths(st_intersects(shp_wphu_outline, centroid)) == 0) {
          shinyalert::shinyalert("Data centroid is outside of the WPHU boundary", type = "warning")
        }
        rdeck::rdeck_proxy("map") |>
          rdeck::update_scatterplot_layer(
            id = paste0("data_centroid_", i),
            data = CoW::find_weighted_centroid(raw_data()[[i]], longitude, latitude, person_weight) |>
              CoW::convert_points_to_sfc(tooltip = "Data", longitude_col = "centre_longitude", latitude_col = "centre_latitude"),
            get_position = geometry,
            get_radius = 1,
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1,
            name = tools::file_path_sans_ext(unique(raw_data()[[i]]$filename))
          )

        env$map <- env$map |>
          rdeck::update_scatterplot_layer(
            id = paste0("data_centroid_", i),
            data = CoW::find_weighted_centroid(raw_data()[[i]], longitude, latitude, person_weight) |>
              CoW::convert_points_to_sfc(tooltip = "Data", longitude_col = "centre_longitude", latitude_col = "centre_latitude"),
            get_position = geometry,
            get_radius = 1,
            get_fill_color = "#63C5DA",
            get_line_color = "#000000ff",
            get_line_width = 1,
            name = tools::file_path_sans_ext(unique(raw_data()[[i]]$filename))
          )
      }
    })
  })


  observe({
    lapply(seq_along(raw_data()), function(i) {
      data_name <- paste0("data_", i)
      req(input[[data_name]], cancelOutput = TRUE)
      rdeck::rdeck_proxy("map") |>
        rdeck::update_scatterplot_layer(
          id = data_name,
          get_fill_color = input[[paste0("data_colour_", i)]]
        ) |>
        rdeck::update_scatterplot_layer(
          id = paste0("data_centroid_", i),
          get_fill_color = input[[paste0("data_colour_", i)]]
        )

      env$map <- env$map |>
        rdeck::update_scatterplot_layer(
          id = data_name,
          get_fill_color = input[[paste0("data_colour_", i)]]
        ) |>
        rdeck::update_scatterplot_layer(
          id = paste0("data_centroid_", i),
          get_fill_color = input[[paste0("data_colour_", i)]]
        )
    })
  })

  inputPanels <- lapply(1:10, function(i) {
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
  })

  output$fileInputs <- renderUI({
    inputPanels[1:num_data_files()] |> tagList()
  })

  # Fires when the + button is pressed
  # Adds extra layers to the map, but only if they don't exist (using `check_layer_name`)
  # Make sure both data and data_centroid layers added.
  observeEvent(
    input$dataIncrease,
    {
      num_data_files(num_data_files() + 1)

      layer_name <- paste0("data_", num_data_files())
      centroid_layer_name <- paste0("data_centroid_", num_data_files())
      if (!check_layer_name(rdeck::rdeck_proxy("map"), layer_name)) {
        rdeck::rdeck_proxy("map") |>
          CoW::add_data_layer(
            id = layer_name,
            dataset_num = num_data_files()
          ) |>
          CoW::add_data_centroid_layer(
            id = centroid_layer_name,
            dataset_num = num_data_files()
          )

          env$map <- env$map |>
          CoW::add_data_layer(
            id = layer_name,
            dataset_num = num_data_files()
          ) |>
          CoW::add_data_centroid_layer(
            id = centroid_layer_name,
            dataset_num = num_data_files()
          )
      }
    }
  )
  # Same as adding but fires when the - button is pressed
  # Note we don't remove layers, maybe we should set `visiblity = FALSE`?
  observeEvent(
    input$dataDecrease,
    {
      if (num_data_files() >= 2) {
        num_data_files(num_data_files() - 1)
      }
    }
  )

  output$mapdownload <- downloadHandler(
    filename = function() {
      paste("wphu-center-map", Sys.Date(), ".html", sep = "")
    },
    content = function(con) {
      htmlwidgets::saveWidget(
        widget = env$map,
        file = con
      )
    }
  )

  observe({
    rdeck::rdeck_proxy("map") |>
      rdeck::add_scatterplot_layer(
        id = "legend",
        name = "Legend",
        visibility_toggle = FALSE,
        data = combined_data_for_legend(),
        get_position = geometry,
        get_fill_color = scale_color_category(
          palette = combined_data_for_legend()$color,
          col = tooltip
        )
      )

    env$map <- env$map |>
      rdeck::add_scatterplot_layer(
        id = "legend",
        name = "Legend",
        visibility_toggle = FALSE,
        data = combined_data_for_legend(),
        get_position = geometry,
        get_fill_color = scale_color_category(
          palette = combined_data_for_legend()$color,
          col = tooltip
        )
      )
  })
}

# Remove airports from mapbox
# Fix download button



## Since last time
# Strip out long/lat into joins
# Normalise to a maximum size. (Actually just capped it but i think it's good enough)
# Postcode templates
