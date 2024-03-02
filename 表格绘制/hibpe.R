library(readxl)
library(ggplot2)
library(dplyr)

# 读取xlsx文件
data <- read_excel("/Users/liyanjia/Desktop/大创/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/Data_CHARLS_2015.xlsx")

# 筛选出肌酐列、胱抑素C列和高血压列数据都不为空的样本
data_eGFR <- data[complete.cases(data[c(366, 373, 118)]), ]

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

# 进行ANOVA分析
anova_result <- aov(r3hibpe ~ group, data = data_eGFR)

# 打印ANOVA结果
print(summary(anova_result))

# 绘制小提琴图
ggplot(data_eGFR, aes(x = group, y = r3hibpe)) +
    geom_violin() +
    labs(title = "Violin plot of r3hibpe by group",
             x = "Group",
             y = "r3hibpe")