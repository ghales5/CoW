## creating WPHU LGA dataset
# Read lga shapefile
shp_lga <- st_read("data-raw/WPHU.LGA/WPHU_LGAs.shp")

# Read lga estimate resident population
erp_wphu_lga <- read.csv("data-raw/ERP_LGA.csv") %>%
  filter(LPHU == "WPHU")

# Get centroids of LGAs, population data and weighted lat/long
shp_wphu_mb <- shp_lga %>%
  dplyr::mutate(centroids = sf::st_centroid(st_geometry(.)),
    x = unlist(map(centroids, 1)),
    y = unlist(map(centroids, 2))
  ) %>%
  dplyr::inner_join(erp_wphu_lga, by = c("LGA_NAME22" = "LGA")) %>%
  dplyr::mutate(
    Xw = (x * Person),
    Yw = (y * Person)
  )


usethis::use_data(shp_wphu_lga, overwrite = TRUE)
