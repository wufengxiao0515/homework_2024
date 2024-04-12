#---------------------------------------------------------------------------------
#script Name: data_exploration.R
#Purpose: This illustrats how to to remove the sites with missing data of the Doubs dataset, and detect whether environmental factors are collinearity,and analysis the relationships between fishes and environment factors and visualize such relationships.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-04-12
#---------------------------------------------------------------------------------
# 加载所需的库

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
