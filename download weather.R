library(FedData)
library(sf)
library(dplyr)
#library(mapview)
library(terra)
#library(lubridate)

#unique_coords (id, lat, lon)
locations$Date <- as.Date(locations$t1_)
unique_coords <- locations[,c("id", "x1_", "y1_","Date")]
unique_coords <- subset(unique_coords, Date <= "2024-12-31")


points_sf <- st_as_sf(unique_coords, coords = c("x1_", "y1_"), crs = 3154)
hull_sf <- st_convex_hull(st_union(points_sf))
study_area_buffered <- st_buffer(hull_sf, 5000)
study_area84 <- st_transform(study_area_buffered, 4326)

#study_area84 <- st_transform(study_area_buffered, crs = 4326)
# mapview(study_area84, color = "red", alpha.regions = 0.2)+
#   mapview(points_to_check, zcol = "id", cex = 2)
# saveRDS(study_area84, "yukon_study_area.rds")

small_files <- list.files("C:/project/Survival/yukon_crop", full.names = TRUE)
swe_stack <- rast(small_files)

daymet_dates <- do.call(c, lapply(2010:2024, function(yr) {
  d <- seq(as.Date(paste0(yr, "-01-01")), as.Date(paste0(yr, "-12-31")), by="day")
  if (lubridate::leap_year(yr)) {
    return(d[1:365]) 
  } else {
    return(d)
  }
}))

names(swe_stack) <- as.character(daymet_dates)

points_sf$date_match <- points_sf$Date
leap_indices <- which(leap_year(points_sf$date_match) & 
                        month(points_sf$date_match) == 12 & day(points_sf$date_match) == 31)
if (length(leap_indices) > 0) {
  points_sf$date_match[leap_indices] <- points_sf$date_match[leap_indices] - days(1)
  message(" ")
}

layer_indices <- match(points_sf$date_match, daymet_dates)

if(any(is.na(layer_indices))) {
  warning("注意：有部分点的日期超出了 SWE 数据的范围，这些点将提取出 NA。")
}

points_lcc <- st_transform(points_sf, crs(swe_stack))
extract_values <- extract(swe_stack, points_lcc)
#cell_ids <- cellFromXY(swe_stack, st_coordinates(points_lcc))

extract_matrix <- cbind(1:length(layer_indices), layer_indices)

extract_value <- extract_values[,-1]
points_sf$daily_swe <- extract_value[extract_matrix]


coords <- st_coordinates(points_sf)
points_sf$x_sf <- coords[, 1]
points_sf$y_sf <- coords[, 2]
points_df_clean <- st_drop_geometry(points_sf)

rm(coords)
rm(extract_matrix)
rm(extract_value)
rm(extract_values)
rm(locations)
rm(points_lcc)
rm(points_sf)
rm(study_area_buffered)
rm(study_area84)
rm(unique_coords)
rm(layer_indices)
rm(swe_stack)
