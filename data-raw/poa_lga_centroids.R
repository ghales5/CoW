poa_lga_centroids <- rbind(
    CoW::shp_wphu_lga %>%
        select(LGA_NAME22, centroids) %>%
        as_tibble() %>%
        select(-geometry) %>%
        mutate(
            longitude = st_coordinates(centroids)[, 1],
            latitude = st_coordinates(centroids)[, 2]
        ) %>%
        select(-centroids) %>%
        rename(postcode_or_lga = LGA_NAME22),
    CoW::shp_wphu_poa %>%
        select(POA_NAME21, centroids) %>%
        as_tibble() %>%
        select(-geometry) %>%
        mutate(
            longitude = st_coordinates(centroids)[, 1],
            latitude = st_coordinates(centroids)[, 2]
        ) %>%
        select(-centroids) %>%
        rename(postcode_or_lga = POA_NAME21)
)

usethis::use_data(poa_lga_centroids, overwrite = TRUE)
