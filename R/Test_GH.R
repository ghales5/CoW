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

wphu_shp <- st_read('data-raw/WPHU.LGA/WPHU_LGAs.shp')
wphu_erp <- read.csv('data-raw/ERP_LGA.csv')

# from 'How to calculate the centroid of a polygon shape file in r'
wphu_shp2 <- wphu_shp %>%
  mutate(centroids = st_centroid(st_geometry(.)))

