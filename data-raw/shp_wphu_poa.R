## creating WPHU LGA dataset
# Read lga shapefile
shp_poa <- st_read("data-raw/WPHU.POA/WPHU_Postcodes.shp")

# Read lga estimate resident population
erp_wphu_poa <- read.csv("data-raw/ERP_POA.csv") %>%
  filter(LPHU == "WPHU")

# Get centroids of Postcodes, population data and weighted lat/long
shp_wphu_poa <- shp_poa %>%
  dplyr::mutate(
    centroids = sf::st_centroid(st_geometry(.)),
    x = unlist(map(centroids, 1)),
    y = unlist(map(centroids, 2))
  ) %>%
  dplyr::inner_join(erp_wphu_poa, by = c("Postcode" = "Postcode")) %>%
  dplyr::mutate(
    Xw = (x * Population),
    Yw = (y * Population)
  )


usethis::use_data(shp_wphu_poa, overwrite = TRUE)
