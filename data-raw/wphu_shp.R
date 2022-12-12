
wphu_shp <- st_read("data-raw/WPHU.LGA/WPHU_LGAs.shp")

usethis::use_data(wphu_shp, overwrite = TRUE)

# Creates shapefiles for the entire WPHU Shapefile
# without the LGA lines.
wphu_outline <- wphu_shp %>%
    st_make_valid() %>%
    st_union()

usethis::use_data(wphu_outline, overwrite = TRUE)