# Test

# =================== LGAs =================================

# Gabby to move this to data-raw
wphu_erp <- read.csv("data-raw/ERP_LGA.csv")

# # Get centroids of LGAs, population data and weighted lat/long
# wphu_shp2 <- wphu_shp %>%
#   mutate(centroids = st_centroid(st_geometry(.)),
#          x = unlist(map(centroids,1)),
#          y = unlist(map(centroids,2)))%>%
#   inner_join(wphu_erp, by=c("ABB_NAME" = "LGA")) %>%
#   mutate(Xw = (x*Population),
#          Yw = (y*Population))
#
# # Calculate the weighted centroid
# weighted_centre <- wphu_shp2 %>%
#   summarise(sXw = sum(Xw),
#             sYw = sum(Yw),
#             sP = sum(Population),
#             longitude = sXw/sP,
#             latitude = sYw/sP)

# Create the WPHU outline and find centroid


centroid <- as.data.frame(st_centroid(st_geometry(wphu_outline))) %>%
  mutate(
    longitude = unlist(map(geometry, 1)),
    latitude = unlist(map(geometry, 2))
  )

# # Plot together
# wphu_points <- wphu_shp %>%
#   ggplot()+
#   geom_sf() +
#   geom_point(data=centroid, aes(x=longitude, y=latitude, colour = 'centre')) +
#   geom_point(data=weighted_centre, aes(x=longitude, y=latitude, colour = 'weighted centre'), shape = 17) +
#   ggtitle("The centres of WPHU, centre \n and population weight centre")
#
# wphu_points

# =================== Meshblocks ================================= More accurate

# Calculate the weighted centroid


# Plot together
wphu_points_MB <- wphu_shp %>%
  ggplot() +
  geom_sf() +
  geom_point(data = centroid, aes(x = longitude, y = latitude, colour = "centre")) +
  geom_point(data = weighted_centre_MB, aes(x = longitude, y = latitude, colour = "weighted centre"), shape = 17) +
  ggtitle("The centres of WPHU, centre \n and population weight centre")

wphu_points_MB ## what3words = Fever.scouts.exams