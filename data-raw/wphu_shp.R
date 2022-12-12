
shp_wphu <- st_read("data-raw/WPHU.LGA/WPHU_LGAs.shp")

usethis::use_data(shp_wphu, overwrite = TRUE)

# Creates shapefiles for the entire WPHU Shapefile
# without the LGA lines.
shp_wphu_outline <- shp_wphu %>%
    st_make_valid() %>%
    st_union()

usethis::use_data(shp_wphu_outline, overwrite = TRUE)
