library(stats)
library(MASS)
library(boot)
library(ResourceSelection)
library(epiDisplay)
library(gtsummary)

# 读取数据文件
data <- read.csv("/Users/liyanjia/Downloads/baseline.csv")
data$Group1 <- factor(data$Group1, levels = c("eGFR_diff negative (<-15 mL/min/1.73 m2)", "eGFR_diff midrange(-15 to 15 mL/min/1.73 m2)", "eGFR_diff positive(≥15 mL/min/1.73 m2)"))

# 使用glm函数拟合logistic回归模型
glm1 <- glm(HighBloodPressure==0~Group1, family = binomial, data = data)
glm2 <- glm(HighBloodPressure==0~Group1 + Age + Gender, family = binomial, data = data)
glm3 <- glm(HighBloodPressure==0~Group1 + Age + Gender + BMI + WaistCircumferance + Smoking + Drink, family = binomial, data = data)

tbl_regression(glm1, 
               exponentiate=T)
tbl_regression(glm2, 
               exponentiate=T)
tbl_regression(glm3,
                exponentiate=T)