#---------------------------------------------------------------------------------
#script Name: data_exploration.R
#Purpose: This illustrats how to to remove the sites with missing data of the Doubs dataset, and detect whether environmental factors are collinearity,and analysis the relationships between fishes and environment factors and visualize such relationships.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-04-12
#---------------------------------------------------------------------------------
#加载所需的库

library(corrplot)
library(ade4)
library(car)

# 加载数据集

data(doubs)
fishdata<-doubs$fish
envdata<-doubs$env

# 检查数据集的缺失值并移除含有缺失值的行

fishdata_complete <- na.omit(fishdata)
envdata_complete<-na.omit(envdata)

# 计算环境因素之间的相关性

correlation_matrix <- cor(envdata_complete)

# 可视化环境因素之间的相关性

corrplot(correlation_matrix, method = "color",
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Environmental Factors")

#用方差膨胀因子VIF来检查环境因素之间的共线性

vif_results <- vif(lm(dfs~alt+slo+flo+pH+har+pho+nit+amm+oxy+bdo,data=envdata_complete))
print(vif_results)

#分别绘制每个环境因素与每种鱼类的关系

for (i in 1:11) {
  for(j in 1:length(fishdata_complete)){
    plot(envdata_complete[, i], fishdata_complete[,j], 
         xlab = colnames(envdata_complete)[i], ylab =colnames(fishdata_complete)[j],
         main = paste("Relationship between", colnames(envdata_complete)[i], "and",colnames(fishdata_complete[j])))
  }
}

#计算鱼类与环境因素之间的相关性
#计算鱼类数量与环境因素之间的相关性

fish_data <- doubs$fish
env_data <- doubs$env
fish_env_cor <- cor(fish_data, env_data)

#输出相关性矩阵

print(fish_env_cor)

#可视化相关性矩阵

library(corrplot)
corrplot(fish_env_cor, method = "color")

#主成分分析
#加载所需包

install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

#提取环境因素数据

env_data <- doubs$env[, c("dfs", "alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bdo")]

#进行主成分分析

pca_result <- PCA(env_data, graph = FALSE)

#提取主成分分析结果

pca_var <- get_pca_var(pca_result)
pca_ind <- get_pca_ind(pca_result)

#绘制主成分分析的解释方差比例图

fviz_eig(pca_result, addlabels = TRUE)

#绘制主成分分析的因子负荷量图

fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#绘制主成分分析的个体得分图

fviz_pca_ind(pca_result, geom = "point", habillage = doubs$fish)
