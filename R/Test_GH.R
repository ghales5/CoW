#Test

# Clear the variables
rm(list = ls())
myPaths <- .libPaths()
pathway <- "C:/RLibraries"
myPaths <- c(myPaths,pathway )
.libPaths(myPaths)


# Load the packages
library(tidyverse)
library(sf)

wphu_shp <- st_read('../data-raw/WPHU.LGA/WPHU_LGAs.shp')
wphu_erp <- read.csv('../data-raw/ERP_LGA.csv')

# Get centroids of LGAs, population data and weighted lat/long
wphu_shp2 <- wphu_shp %>%
  mutate(centroids = st_centroid(st_geometry(.)),
         x = unlist(map(centroids,1)),
         y = unlist(map(centroids,2)))%>%
  inner_join(wphu_erp, by=c("ABB_NAME" = "LGA")) %>%
  mutate(Xw = (x*Population),
         Yw = (y*Population))
  
# Calculate the weighted centroid         
weighted_centre <- wphu_shp2 %>%
  summarise(sXw = sum(Xw),
            sYw = sum(Yw),
            sP = sum(Population),
            longitude = sXw/sP,
            latitude = sYw/sP)

wphu_pop_centre <- wphu_shp %>%
  ggplot()+
  geom_sf() +
  geom_point(data=weighted_centre, aes(x=longitude, y=latitude), colour="red")

wphu_pop_centre
