library(readxl)
library(ggplot2)
library(dplyr)

# 读取xlsx文件
data <- read_excel("/Users/liyanjia/Desktop/大创/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/Data_CHARLS_2015.xlsx")

# 筛选出第126列、366列、373列数值都不为空的样本
data_eGFR <- data[complete.cases(data[[366]], data[[373]]), ]


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
    }
)

# 将GFR_diff添加到data_eGFR的最后一列
data_eGFR$GFR_diff <- GFR_diff
# 根据GFR_diff的值创建group列
data_eGFR$group <- ifelse(data_eGFR$GFR_diff < -15, "-", ifelse(data_eGFR$GFR_diff > 15, "+", "0"))

# 从data_eGFR中获取data_hearte, data_hibpe, data_diabe
data_hearte <- data_eGFR[complete.cases(data_eGFR$r3hearte, data_eGFR$GFR_diff), ]
data_hibpe <- data_eGFR[complete.cases(data_eGFR$r3hibpe, data_eGFR$GFR_diff), ]
data_diabe <- data_eGFR[complete.cases(data_eGFR$r3diabe, data_eGFR$GFR_diff), ]


cor(data_hearte$GFR_diff, data_hearte$r3hearte, method = "pearson", use = "complete.obs")
cor(data_hearte$GFR_diff, data_hearte$r3hibpe, method = "pearson", use = "complete.obs")
cor(data_hearte$GFR_diff, data_hearte$r3diabe, method = "pearson", use = "complete.obs")
# 分别根据r3hearte, r3hibpe, r3diabe的值将GFR_diff分为两组
GFR_diff_hearte_0 <- data_hearte$GFR_diff[data_hearte$r3hearte == 0]
GFR_diff_hearte_1 <- data_hearte$GFR_diff[data_hearte$r3hearte == 1]
GFR_diff_hibpe_0 <- data_hearte$GFR_diff[data_hearte$r3hibpe == 0]
GFR_diff_hibpe_1 <- data_hearte$GFR_diff[data_hearte$r3hibpe == 1]
GFR_diff_diabe_0 <- data_hearte$GFR_diff[data_hearte$r3diabe == 0]
GFR_diff_diabe_1 <- data_hearte$GFR_diff[data_hearte$r3diabe == 1]

# 分别对每两组数据进行t检验
t.test(GFR_diff_hearte_0, GFR_diff_hearte_1)
t.test(GFR_diff_hibpe_0, GFR_diff_hibpe_1)
t.test(GFR_diff_diabe_0, GFR_diff_diabe_1)


# 绘制箱线图
data_hearte$group <- as.factor(data_hearte$group)
ggplot(data_hearte, aes(x = group, y = mean(r3hearte))) +
  geom_boxplot(fill = "gray") +
  labs(title = "possibility of hearte disease between different eGFR groups", x = "eGFR_diff", y = "r3hearte") +
  theme_classic()

if (FALSE) {
data_hearte$r3hearte <- as.factor(data_hearte$r3hearte)
ggplot(data_hearte, aes(x = r3hearte, y = GFR_diff)) +
    geom_boxplot(fill = "gray") +
    labs(title = "Relationship between r3hearte and GFR_diff", x = "r3hearte", y = "GFR_diff") +
    theme_classic()
}
if (FALSE) {
data_hibpe$r3hibpe <- as.factor(data_hibpe$r3hibpe)
ggplot(data_hibpe, aes(x = r3hibpe, y = GFR_diff)) +
    geom_boxplot(fill = "gray") +
    labs(title = "Relationship between r3hibpe and GFR_diff", x = "r3hibpe", y = "GFR_diff") +
    theme_classic()
}
if (FALSE) {
data_diabe$r3diabe <- as.factor(data_diabe$r3diabe)
ggplot(data_diabe, aes(x = r3diabe, y = GFR_diff)) +
    geom_boxplot(fill = "gray") +
    labs(title = "Relationship between r3diabe and GFR_diff", x = "r3diabe", y = "GFR_diff") +
    theme_classic()
}
  