library(sf)

# Mapping

# Reading in the file

ward_shapefile <- st_read("./Wards_December_2022_Boundaries_UK_BGC_2278287139269798378/WD_DEC_2022_UK_BGC.shp")

# Filtering the wards for Bradford from the shapefile

ward_shapefile_Bradford <- ward_shapefile %>%
  filter(LAD22NM %in% "Bradford")

# Joining the shapefile and the camera data

street_all_sites_2 <- left_join(ward_shapefile_Bradford, street_all_sites_2, by = c("WD22CD" = "Ward-Code"))

