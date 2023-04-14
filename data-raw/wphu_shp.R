# Creates shapefiles for the entire WPHU Shapefile
# without the LGA lines.
shp_wphu_outline <- shp_wphu_lga %>%
    st_make_valid() %>%
    st_union()

usethis::use_data(shp_wphu_outline, overwrite = TRUE)
