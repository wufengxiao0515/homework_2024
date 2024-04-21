#---------------------------------------------------------------------------------
#script Name: geodata_,manipul.R
#Purpose: This displays：1.set 2-km buffer along the Doubs river and clip from the map to extract the raster values of the catchment area and slope for each point with the qgisprocess package.2.merge the extracted data with other environmental factors from Doubs dataset to form a dataframe, and finally transfer the dataframe to a sf object, which contains a geometry column.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-04-21
#---------------------------------------------------------------------------------

library(qgisprocess)
library(raster)
library(sf)

#绘制高程图

elevation<-raster("D://吴凤箫//homework//map//map.tif")

plot(elevation)

#读取点数据，按照样点裁切高程数据

pts<-doubs_path <- st_read("D://吴凤箫//homework//map//points.shp")
crs(pts)#查看点坐标系
extent(pts)#查看样点范围
crs(elevation)#查看高程坐标系
extent(elevation)#查看高程地理范围
E<-extent(pts)+c(-2000,2000,-2000,2000)#每个点周围增加2000m缓冲区
elevation_crop<-crop(elevation,E)#用带有一定面积缓冲区裁切高程数据
plot(elevation_crop)#绘制裁切后的高程图

#计算地形参数并绘图

slope<-terrain(elevation_crop,"slope")#计算坡度
flow_dir_raster <- terrain(elevation_crop, opt = "flowdir")#计算流向栅格数据
flow_accum_raster <- area(flow_dir_raster)#计算流域面积
topo<-stack(elevation_crop,slope,flow_accum_raster,flow_dir_raster)#生成栅格线
plot(topo)

#提取地形参数的数值

flow_accum_values <- extract(flow_accum_raster, pts)
slope_values <- extract(slope, pts)

#加载doubs其他的环境变量

library(ade4)
data(doubs)
write.csv(doubs$env,file = "D://吴凤箫//homework//map//env.csv")
other_factors <- read.csv("D://吴凤箫//homework//map//env.csv")
place<-read.csv("D://吴凤箫//homework//homework.csv")

# 创建两个 30 行 12 列的空数据框

new_data_slope <- matrix(NA, nrow = 30, ncol = 12)
new_data_flow <- matrix(NA, nrow = 30, ncol = 12)

# 将 25 个数据填充到数据框中，按照列优先的顺序填充

new_data_slope[1:25] <- slope_values
new_data_flow[1:25] <- flow_accum_values

# 将矩阵转换为数据框

new_data_df_slope <- as.data.frame(new_data_slope)
new_data_df_flow <- as.data.frame(new_data_flow)

# 将所有向量合并到一个数据框中
merged_data <- data.frame(new_data_df_slope, new_data_df_flow, other_factors,place)

# 将数据框转换为 sf 对象
doubs_sf <- st_as_sf(merged_data, coords = c("Longitude", "Latitude"))

# 保存 sf 对象
st_write(doubs_sf, "doubs_sf.shp")
