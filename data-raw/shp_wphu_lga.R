# TODO: Add population to this shapefile.
shp_wphu_lga <- st_read("data-raw/WPHU.LGA/WPHU_LGAs.shp")

usethis::use_data(shp_wphu_lga, overwrite = TRUE)