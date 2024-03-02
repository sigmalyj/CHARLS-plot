library(readxl)
library(ggplot2)
library(dplyr)
library(gtsummary)

col = seq(118, 144, 2)
# 读取xlsx文件
data <- read_excel("/Users/liyanjia/Desktop/大创/raw_data/CHARLS_data_variable_2011-2020/CHARLS_分库_2011-2020/Data_CHARLS_2015.xlsx")


# 筛选出r3agey列、ragender列、bl_crea列、bl_cysc列和十四种疾病列数据都不为空的样本
data_eGFR <- data[complete.cases(data[c(24, 26, 366, 373, col)]), ]

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

# 根据GFR_diff的值创建group1列
data_eGFR$group1 <- ifelse(data_eGFR$GFR_diff < -15, "-", ifelse(data_eGFR$GFR_diff > 15, "+", "0"))

# 对于data_eGFR中的每一个样本都计算其GFR_cr除以GFR_cys
CCR <- apply(data_eGFR, 1, function(x) {
  creatinine <- as.numeric(x[366])
  age <- as.numeric(x[24])
  sex <- as.numeric(x[26])
  cystatin <- as.numeric(x[373])
  
  return(GFR_cr(creatinine, age, sex)/GFR_cys(cystatin, age))
})

# 将CCR添加到data_eGFR的最后一列
data_eGFR$CCR <- CCR

# 计算CCR的三分位数
quantiles <- quantile(data_eGFR$CCR, probs = c(1/3, 2/3))

# 添加group2列
data_eGFR <- data_eGFR %>% 
  mutate(group2 = cut(CCR, breaks = c(-Inf, quantiles[1], quantiles[2], Inf), labels = c("1", "2", "3")))

# 计算TyG
tyg = log(data_eGFR$bl_glu * data_eGFR$bl_tg / 2)

# 创建baseline数据框，并删除含有缺失值的行
baseline <- data_eGFR %>% 
    transmute(
        Age = r3agey, 
        Gender = ragender, 
        BMI = r3mbmi, 
        WC = r3mwaist,
        Creatinine = bl_crea, 
        CystatinC = bl_cysc, 
        GFR_diff = GFR_diff, 
        CCR = CCR,
        TyG = tyg,
        Group1 = group1,
        Group2 = group2,
        Smoking = r3smoken,
        Drink = r3drinkr_c,
        WBC = bl_wbc,
        HGB = bl_hgb,
        HCT = bl_hct,
        PLT = bl_plt,
        TG = bl_tg,
        TopCodingTG = bl_top_coding_tg,
        BUN = bl_bun,
        HDL = bl_hdl,
        LDL = bl_ldl,
        CHO = bl_cho,
        GLU = bl_glu,
        UA = bl_ua,
        CRP = bl_crp,
        HBALC = bl_hbalc,
        HighBloodPressure = r3hibpe,
        Diabete = r3diabe,
        Stroke = r3stroke,
        Dyslipoproteinemia = r3dyslipe,
        Cancer = r3cancre,
        LungDisease = r3lunge,
        HeartDisease = r3hearte,
        PsychiatricDisorder = r3psyche,
        Arthritis = r3arthre,
        LiverDisease = r3livere,
        KidneyDisease = r3kidneye,
        DigestiveDisorder = r3digeste,
        Asthma = r3asthmae,
        MemoryLoss = r3memrye
    ) %>% 
    mutate(Gender = ifelse(Gender == 1, "male", "female")) %>% 
    mutate(Gender = factor(Gender, levels = c("male", "female"))) %>% 
    mutate(Group1 = factor(Group1, levels = c("-", "0", "+"), labels = c("eGFR_diff negative (<-15 mL/min/1.73 m2)", "eGFR_diff midrange(-15 to 15 mL/min/1.73 m2)", "eGFR_diff positive(≥15 mL/min/1.73 m2)"))) %>%
    mutate(Group2 = factor(Group2, levels = c("1", "2", "3"), labels = c("CCR tile 1", "CCR tile 2", "CCR tile 3"))) %>% 
    rename(
        WaistCircumferance = WC,
        WhiteBloodCells = WBC,
        Hemoglobin = HGB,
        Hematocrit = HCT,
        Platelets = PLT,
        Triglycerides = TG,
        TopCodingTriglycerides = TopCodingTG,
        BloodUreaNitrogen = BUN,
        HighDensityLipoprotein = HDL,
        LowDensityLipoprotein = LDL,
        Cholesterol = CHO,
        Glucose = GLU,
        UricAcid = UA,
        CReactiveProtein = CRP,
        HemoglobinA1c = HBALC
    ) %>%
  na.omit()

write.csv(baseline, "baseline.csv", row.names = FALSE)

# 打印baseline数据框的摘要
baseline %>% 
  tbl_summary(by = Group1) %>%
  add_overall() %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  modify_caption("Baseline Characteristics of Participants according to three categories of eGFRdiff level in CHARLS 2015.")


