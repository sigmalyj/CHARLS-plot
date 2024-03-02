library(readxl)
library(dplyr)
library(survival)
library(survminer)

# 读取xlsx文件
data <- read_excel("/Users/liyanjia/Desktop/大创/raw_data/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/Data_CHARLS_2015.xlsx")

# 筛选出r3agey列、ragender列、bl_crea列、bl_cysc列数据都不为空的样本
data_eGFR <- data[complete.cases(data[c(12, 14, 24, 26, 366, 373)]), ]

# CKD-EPI creatinine equation for estimated GFR
GFR_cr <- function(creatinine, age, sex) {
  if (sex == 2) {
    if (creatinine <= 0.7) {
      gfr <- 144 * (creatinine / 0.7)^-0.329 * 0.993^age
    } else {
      gfr <- 144 * (creatinine / 0.7)^-1.209 * 0.993^age
    }
  } else {
    if (creatinine <= 0.9) {
      gfr <- 141 * (creatinine / 0.7)^-0.411 * 0.993^age
    } else {
      gfr <- 141 * (creatinine / 0.7)^-1.209 * 0.993^age
    }
  }
  
  return(gfr)
}

# CKD-EPI cystatin C equation for estimating GFR
GFR_cys <- function(cystatin, age) {
  if (cystatin <= 0.8) {
    gfr <- 133 * (cystatin / 0.8)^-0.499 * 0.996^age
  } else {
    gfr <- 133 * (cystatin / 0.8)^-1.328 * 0.996^age
  }
  
  return(gfr)
}

# 对于data_eGFR中的每一个样本都计算其GFR_cys减去GFR_cr
GFR_diff <- apply(data_eGFR, 1, function(x) {
  creatinine <- as.numeric(x[366])
  age <- as.numeric(x[24])
  sex <- as.numeric(x[26])
  cystatin <- as.numeric(x[373])
  
  return(GFR_cys(cystatin, age) - GFR_cr(creatinine, age, sex))
})

# 将GFR_diff添加到data_eGFR的最后一列
data_eGFR$GFR_diff <- GFR_diff

# 根据GFR_diff的值创建group列
data_eGFR$group <- ifelse(data_eGFR$GFR_diff < -15, "-", ifelse(data_eGFR$GFR_diff > 15, "+", "0"))

# 提取出所需要的数据
data_eGFR <- data_eGFR %>%
  transmute(ID,
            group,
            y = r3iwy,
            m = r3iwm)

death_data <- read_excel("/Users/liyanjia/Desktop/大创/raw_data/CHARLS2020r/exit.xlsx")
death_data = death_data[complete.cases(death_data$exb001_1), ]
death_data <- death_data %>% 
  transmute(ID, 
            dy = exb001_1,
            dm = exb001_2,
            era = exb002,) %>% 
  mutate(era = factor(era, levels = c("1 Solar calendar", "2 Lunar calendar"), labels = c("1", "2")))

# 将data和death_data合并
surv_data <- merge(data_eGFR, death_data, by = "ID")
# 计算每个样本的生存时间
surv_data$months <- (surv_data$dy - surv_data$y) * 12 + (surv_data$dm - surv_data$m)

# 创建Surv对象
surv_obj <- Surv(surv_data$months)

# 使用survfit函数拟合生存模型
fit <- survfit(surv_obj ~ surv_data$group)

# 使用ggsurvplot函数绘制生存曲线
ggsurvplot(fit, data = surv_data, risk.table = TRUE, legend.labs = NULL, xlab = "Time(months)")

# 使用survdiff函数进行log-rank检验
test_result <- survdiff(surv_obj ~ surv_data$group)

# 输出p值
p_value <- pchisq(test_result$chisq, length(test_result$n) - 1, lower.tail = FALSE)
print(p_value)