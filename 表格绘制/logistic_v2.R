library(stats)
library(MASS)
library(boot)
library(ResourceSelection)
library(epiDisplay)

# 读取数据文件
data <- read.csv("/Users/liyanjia/Downloads/baseline.csv")
# 移除不需要的列
data$Group1 <- NULL
data$Group2 <- NULL

# 使用glm函数拟合logistic回归模型
glm_model1 <- glm(HighBloodPressure ~ TyG, family = binomial, data = data)
glm_model2 <- glm(HighBloodPressure ~ TyG + Age + Gender, family = binomial, data = data)
glm_model3 <- glm(HighBloodPressure ~ TyG + Age + Gender + BMI + WaistCircumferance + Smoking + Drink, family = binomial, data = data)

summary(glm_model1)
summary(glm_model2)
summary(glm_model3)

logistic.display(glm_model1)
logistic.display(glm_model2)
logistic.display(glm_model3)