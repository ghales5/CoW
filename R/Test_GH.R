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

#=================== LGAs =================================
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

# Create the WPHU outline and find centroid
wphu_outline <- wphu_shp %>%
  st_make_valid() %>%
  st_union() 

centroid <-  as.data.frame(st_centroid(st_geometry(wphu_outline)))%>%
  mutate(longitude = unlist(map(geometry,1)),
         latitude = unlist(map(geometry,2)))

# Plot together
wphu_points <- wphu_shp %>%
  ggplot()+
  geom_sf() +
  geom_point(data=centroid, aes(x=longitude, y=latitude, colour = 'centre')) +
  geom_point(data=weighted_centre, aes(x=longitude, y=latitude, colour = 'weighted centre'), shape = 17) +
  ggtitle("The centres of WPHU, centre \n and population weight centre")
  
wphu_points

#=================== Meshblocks ================================= More accurate
wphu_MB <- read.csv('../data-raw/MB_WPHU.csv')%>%
  mutate(ID= as.character(MB_CODE_2021))

wphu_MB_erp <- read.csv('../data-raw/MB_Counts.csv') %>%
  mutate(ID= as.character(MB_CODE_2021))

MB_shp <- st_read('../data-raw/MB_2021_AUST_SHP_GDA94/MB_2021_AUST_GDA94.shp')%>%
  filter(MB_CODE21 %in% wphu_MB$ID)

# Get centroids of MBs, population data and weighted lat/long
wphu_mb_shp <- MB_shp %>%
  mutate(centroids = st_centroid(st_geometry(.)),
         x = unlist(map(centroids,1)),
         y = unlist(map(centroids,2)))%>%
  inner_join(wphu_MB_erp, by=c("MB_CODE21" = "ID")) %>%
  mutate(Xw = (x*Person),
         Yw = (y*Person))

# Calculate the weighted centroid         
weighted_centre_MB <- wphu_mb_shp %>%
  summarise(sXw = sum(Xw),
            sYw = sum(Yw),
            sP = sum(Person),
            longitude = sXw/sP,
            latitude = sYw/sP)

# Plot together
wphu_points_MB <- wphu_shp %>%
  ggplot()+
  geom_sf() +
  geom_point(data=centroid, aes(x=longitude, y=latitude, colour = 'centre')) +
  geom_point(data=weighted_centre_MB, aes(x=longitude, y=latitude, colour = 'weighted centre'), shape = 17) +
  ggtitle("The centres of WPHU, centre \n and population weight centre")

wphu_points_MB ## what3words = Fever.scouts.exams
