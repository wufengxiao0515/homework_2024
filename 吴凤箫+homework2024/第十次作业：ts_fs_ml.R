# --------------------------------------------
# Script Name: Timeseries EDA and ML
# Purpose: This script is to show how conduct EDA and ML of
#                time series. The key is to extract
#               the features from timestamps.

# Author:     Fengxiao Wu
# Email:      wfx1876@163.com
# Date:       2024-05-13
#
# --------------------------------------------

#1. 创建时间序列

data=read.table("D://吴凤箫//homework//第十次作业//Prunier et al._RawBiomassData.txt",h=TRUE)
head(data)

#识别并删除重复数据 

library(dplyr)
data_clean <- data |>
  dplyr::select(-YEAR) |>
  # drop_na() |>
  distinct()

#检查数据中的站点和鱼类物种，并输出它们的唯一值。

unique(data_clean$STATION)
table(data_clean$STATION)
unique(data_clean$SP)
table(data_clean$SP)

#以VERCah站鱼类密度数据为中心，选取VAI的鱼类种类

mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

#创建时间序列

data_ts = ts(data = mydata[, -c(1:5)], #选择mydata的所有列作为时间序列数据
             start = c(1994), #指定时间序列的起始年份，这里设置为 1994
             frequency = 1)  #设置时间序列的频率，这里设置为1，表示数据是按年份采集的

#2.时间序列可视化

#绘制时间序列数据的图形，并使用分面功能根据不同的时间序列进行分组显示

library(forecast)
library(ggplot2)
autoplot(data_ts, facets = TRUE) +
  ggtitle("VAI of Doubs river") +
  ylab("Changes") + xlab("Year")#使用 autoplot() 函数绘制时间序列图,标题为 "VAI of Doubs river",分别设置 y 轴和 x 轴的标签为 "Changes" 和 "Year"

#使用timetk绘制时间序列图表

library(timetk)
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI") 

library(tidyverse)
library(tsibble)

datatk_ts <- mydata |>
  tk_tbl() |> #将数据框转换为 tibble 格式
  # mutate(DATE = as_date(as.POSIXct.Date(DATE))) |>
  select(-1) |>#删除第一列
  rename(date = DATE) |>#将名为 "DATE" 的列重命名为 "date"
  relocate(date, .before = STATION) |>#将 "date" 列移到 "STATION" 列之前
  pivot_longer(
    cols = c("BIOMASS", "DENSITY"))#将数据从宽格式转换为长格式，使得 "BIOMASS" 和 "DENSITY" 列的值成为新的行，同时保留其他列不变。

datatk_ts |>
  group_by(name) |># 按照 "name" 列对数据进行分组
  plot_time_series(date, value, 
                   .facet_ncol = 2, # 将图表按照两列进行排列
                   .facet_scale = "free",# 允许每个子图的比例尺自由变化
                   .interactive = FALSE,# 禁用交互式功能
                   .title = "VAI of Le Doubs river"# 设置图表标题
  )#使用 plot_time_series 函数绘制时间序列图表

datatk_ts1 <- 
  datatk_ts |>
  group_by(name) |>
  summarise_by_time(
    date, 
    .by = "year",
    value = first(value)) |>
  pad_by_time(date, .by = "year") |>
  plot_time_series(date, value,
                   .facet_ncol = 2,
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
  )

#对时间序列数据进行维度表示或降维处理，并绘制相应的图表

library(TSrepr)

mydata <- data_clean |>
  filter(STATION=="VERCah" & SP == "VAI")

biom_ts <- ts(mydata[,-c(1:5,7)],
              start= c(1994,1),
              frequency =1)#将筛选后的数据除去前5列和第7列，然后使用 ts() 函数将剩余的数据转换为时间序列对象 biom_ts，起始时间为1994年1月，频率为1。

p1 <- autoplot(biom_ts) +
  ggtitle("VAI biomass of Doubs river") +
  ylab("Changes") + xlab("Year")#使用 autoplot() 函数绘制时间序列数据的图表，并添加标题、y轴标签和x轴标签，保存到变量 p1 中。

data_dwt <- repr_dwt(mydata$BIOMASS, level = 1) 
data_dwt_ts <- ts(data_dwt,
                  start= c(1994,1),
                  frequency =1)#对原始数据中的 "BIOMASS" 列进行小波变换，将结果保存到 data_dwt 中，并指定变换水平为1。然后将变换后的数据转换为时间序列对象 data_dwt_ts，起始时间和频率与原始数据相同。

p2 <- autoplot(data_dwt_ts) +
  ggtitle("VAI biomass of Doubs river") +
  ylab("Changes") + xlab("Year")#再次使用 autoplot() 函数绘制小波变换后的时间序列数据的图表，并添加标题、y轴标签和x轴标签，保存到变量 p2 中。

library(patchwork)
p1+p2#使用 patchwork 包中的函数将两个图表组合在一起并显示出来。

#3.提取时间序列特征

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)

mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

biomtk_ts <- mydata |>
  tk_tbl() |> 
  select(DATE, BIOMASS)#将 mydata 转换为 tibble 格式，并从中选择了日期（DATE）和生物量（BIOMASS）两列数据，并将结果存储在 biomtk_ts 中

# 检查时间序列的规律性

biomtk_ts |>
  tk_summary_diagnostics(.date_var = DATE)

#提取时间序列的所有特征

biomtk_ts_features_all <- biomtk_ts |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_timeseries_signature(.date_var = DATE) |>#添加基于日历的特征
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  # dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>#添加傅里叶特征
  tk_augment_lags(.value = BIOMASS, .lags = c(4,7)) |>#添加滞后特征
  tk_augment_slidify(.value   = contains("BIOMASS"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")#添加移动窗口统计特征

biomtk_ts_features_all |>
  glimpse()

#执行时间序列回归分析

plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_all,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              index.num + year + half + quarter + month + 
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              # BIOMASS_lag4 + BIOMASS_lag7 + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            # BIOMASS_lag4_roll_3 + BIOMASS_lag7_roll_3 + 
                            # BIOMASS_lag4_roll_6 + BIOMASS_lag7_roll_6,
                            .show_summary = TRUE)

#4.利用tidymodels软件包建立预测模型

library(tidyverse)  
library(timetk) 
library(tidymodels)
library(modeltime)
library(timetk)

mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

biomtk_ts <- mydata |> # 
  tk_tbl() |> 
  select(index, DATE, BIOMASS) 

library(tidyquant)
ggplot(biomtk_ts, aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("Biomass of Fishes in Doubs")

#展示训练集和测试集的时间序列数据

n_rows <- nrow(biomtk_ts)
train_rows <- round(0.8 * n_rows)#计算训练集的行数，约为总行数的80%

train_data <- biomtk_ts |>
  slice(1:train_rows) 
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)#提取训练集和测试集数据

ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()#分别绘制训练集和测试集的时间序列曲线

#创建特征，配方预处理

library(recipes)
library(tidymodels)

recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>#创建一个配方，指定目标变量 BIOMASS 与其余所有变量之间的关系
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>#在指定列进行变异操作
  step_timeseries_signature(DATE) |>#在时间序列数据中添加签名特征
  step_rm(DATE) |>#删除DATE列,因为它在之前的步骤中被处理过了
  step_zv(all_predictors()) |>#删除零方差的特征，即那些所有值都相同的特征
  step_dummy(all_nominal_predictors(), one_hot = TRUE)#将分类变量转换为虚拟变量

summary(prep(recipe_spec_final))#汇总配方的预处理过程

#训练和评估模型

#4.1提升树模型

#创建工作流程

bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>#使用 xgboost 引擎的提升树模型
  add_recipe(recipe_spec_final) |>#将数据预处理流程添加到工作流程中。
  fit(train_data)#对训练数据进行模型训练

bt

#评估模型性能

bt_test <- bt |> 
  predict(test_data) |>
  bind_cols(test_data) 

bt_test#生成模型的测试预测结果，并将其与测试数据合并

#绘制训练数据、测试数据以及测试数据的预测结果的时间序列图

pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

#计算预测误差

bt_test |>
  metrics(BIOMASS, .pred)

#4.2 随机森林模型

#创建工作流程

rf <- workflow() |>
  add_model(
    spec = rand_forest("regression") |> set_engine("ranger")
  ) |>#使用 ranger 引擎的随机森林模型
  add_recipe(recipe_spec_final) |>
  fit(train_data)

rf

#评估模型性能

rf_test <- rf |> 
  predict(test_data) |>
  bind_cols(test_data) 

rf_test

#绘制训练数据、测试数据以及测试数据的预测结果的时间序列图

prf <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

#计算预测误差

rf_test |>
  metrics(BIOMASS, .pred)

library(patchwork)
pbt + prf

#4.3比较不同算法

#创建Modeltime表

model_tbl <- modeltime_table(
  bt,
  rf
)

model_tbl

#校准表

calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)

calibrated_tbl 

#模型评估

calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)

#预测图

calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = biomtk_ts,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )



