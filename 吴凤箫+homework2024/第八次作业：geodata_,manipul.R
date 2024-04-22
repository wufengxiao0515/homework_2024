#---------------------------------------------------------------------------------
#script Name: geodata_,manipul.R
#Purpose: This displays：1.set 2-km buffer along the Doubs river and clip from the map to extract the raster values of the catchment area and slope for each point with the qgisprocess package.2.merge the extracted data with other environmental factors from Doubs dataset to form a dataframe, and finally transfer the dataframe to a sf object, which contains a geometry column.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-04-21
#---------------------------------------------------------------------------------

#加载所需的数据包：

library(terra)
library(sf)
library(qgisprocess)
library(ggplot2)
library(dplyr)
library(rgdal)

# 创建2-km缓冲区

# 载入所需数据

doubs_dem <- terra::rast("D://吴凤箫//homework//map//map.tif")
doubs_river <- sf::st_read("D://吴凤箫//homework//map//river.shp")
doubs_points <- sf::st_read("D://吴凤箫//homework//map//points.shp")

# 转换投影坐标系及设置地理坐标系

doubs_river_utm <- st_transform(doubs_river,32631)
doubs_points_pts<-st_transform(doubs_points,32631)
utm_crs <- "EPSG:32631"
doubs_dem_utm <- terra::project(doubs_dem,utm_crs)
terra::crs(doubs_dem_utm)

# 建立缓冲区

doubs_river_buffer <- st_buffer(doubs_river_utm,dist = 2000)
plot(st_geometry(doubs_river_buffer),axes = TRUE)
library(ggplot2)
ggplot() + geom_sf(data = doubs_river_buffer) # 自行转换为地理坐标系

# 裁剪所需高程数据

# 进行裁剪

doubs_dem_utm_cropped = crop(doubs_dem_utm,doubs_river_buffer)
doubs_dem_utm_masked = mask(doubs_dem_utm_cropped,doubs_river_buffer)

# 可视化

plot(doubs_dem_utm_masked,axes =TRUE)

# 提取集水区面积

library(qgisprocess)
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title,algorithm) |>
  head(2)

topo_total = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagwetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1,
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

topo_select <- topo_total[c("AREA","SLOPE")] |>
  unlist() |>
  rast()

names(topo_select) = c("carea","cslope")
origin(topo_select) = origin(doubs_dem_utm_masked)
topo_char = c(doubs_dem_utm_masked,topo_select)
topo_env <- terra::extract(topo_char,doubs_points_utm,ID = FALSE)

# 增加环境数据
# 载入数据

Doubs
water_env <- env

# 增加数据列

doubs_env = cbind(doubs_points_utm,topo_env,water_env)

# 将数据框转换为 sf 对象
doubs_sf <- st_as_sf(doubs_env, coords = c("Longitude", "Latitude"))

# 保存 sf 对象
st_write(doubs_sf, "doubs_sf.shp")
