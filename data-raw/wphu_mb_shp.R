wphu_MB_erp <- read.csv("data-raw/MB_Counts.csv") %>%
    mutate(ID = as.character(MB_CODE_2021))

MB_shp <- st_read("data-raw/MB_2021_WPHU_SHP_GDA94/MB_2021_WPHU_GDA94.shp")

# Get centroids of MBs, population data and weighted lat/long
wphu_mb_shp <- MB_shp %>%
    mutate(
        centroids = st_centroid(st_geometry(.)),
        x = unlist(map(centroids, 1)),
        y = unlist(map(centroids, 2))
    ) %>%
    inner_join(wphu_MB_erp, by = c("MB_CODE21" = "ID")) %>%
    mutate(
        Xw = (x * Person),
        Yw = (y * Person)
    )

usethis::use_data(wphu_mb_shp)

weighted_centre_MB <- wphu_mb_shp %>%
    summarise(
        sXw = sum(Xw),
        sYw = sum(Yw),
        sP = sum(Person),
        centre_longitude = sXw / sP,
        centre_latitude = sYw / sP
    )

usethis::use_data(weighted_centre_MB)