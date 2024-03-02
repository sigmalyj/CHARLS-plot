library(readxl)
library(dplyr)
library(survival)
library(survminer)

# 读取xlsx文件
data <- read_excel("/Users/liyanjia/Desktop/大创/raw_data/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/Data_CHARLS_2015.xlsx")

# 筛选出bl_tg列、bl_glu列都不为空的样本
data_TyG <- data[complete.cases(data[c(364, 371)]), ]

# 计算TyG
tyg = log(data_TyG$bl_glu * data_TyG$bl_tg / 2)