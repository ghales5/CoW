wphu_MB_erp <- read.csv("data-raw/MB_Counts.csv") %>%
    dplyr::mutate(ID = as.character(MB_CODE_2021))

MB_shp <- sf::st_read("data-raw/MB_2021_WPHU_SHP_GDA94/MB_2021_WPHU_GDA94.shp")

# Get centroids of MBs, population data and weighted lat/long
wphu_mb_shp <- MB_shp %>%
    dplyr::mutate(
        centroids = sf::st_centroid(st_geometry(.)),
        x = unlist(map(centroids, 1)),
        y = unlist(map(centroids, 2))
    ) %>%
    dplyr::inner_join(wphu_MB_erp, by = c("MB_CODE21" = "ID")) %>%
    dplyr::mutate(
        Xw = (x * Person),
        Yw = (y * Person)
    )

usethis::use_data(wphu_mb_shp)

weighted_centre_MB <- wphu_mb_shp %>%
    dplyr::select(
        Xw,
        Yw,
        Person
    ) %>%
    tibble::as_tibble() %>%
    dplyr::summarise(
        sXw = sum(Xw),
        sYw = sum(Yw),
        sP = sum(Person),
        centre_longitude = sXw / sP,
        centre_latitude = sYw / sP
    ) %>%
    dplyr::select(
        -sXw,
        -sYw,
        -sP
    )

usethis::use_data(weighted_centre_MB)
