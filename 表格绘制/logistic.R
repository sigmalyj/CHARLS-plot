# 导入所需的包
library(stats)
library(MASS)
library(boot)
library(ResourceSelection)
library(epiDisplay)

# 读取数据文件
data <- read.csv("/Users/liyanjia/Desktop/大创/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/baseline.csv") 
# 移除不需要的列
data$Group1 <- NULL
data$Group2 <- NULL


# 使用glm函数拟合logistic回归模型
glm_model3 <- glm(HighBloodPressure ~ Age + Gender + BMI + WaistCircumferance + GFR_diff + CCR + Smoking + Drink + WhiteBloodCells + Hemoglobin + Hematocrit + Platelets + Triglycerides + TopCodingTriglycerides + BloodUreaNitrogen + HighDensityLipoprotein + LowDensityLipoprotein + Cholesterol + Glucose + UricAcid + CReactiveProtein + HemoglobinA1c, family = binomial, data = data) 

# 使用stepAIC函数选择最优模型
best_model <- stepAIC(glm_model)

# 打印模型摘要
summary(best_model)

# 进行Hosmer-Lemeshow检验
hoslem.test(data$HighBloodPressure, fitted(best_model), g = 10)

# 生成逻辑回归模型的可视化
model = logistic.display(best_model)
# 将模型结果保存为CSV文件
write.csv(model$table, "logistic.csv", row.names = FALSE)