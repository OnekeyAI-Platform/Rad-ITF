library(tableone)
library(openxlsx)
library(openxlsx)
library(readxl)
aa<-read_excel("D:/ITF预测侵袭性复发/肝癌复发数据集/合并训练集.xlsx")
# 1. 明确指定需要显示所有水平的分类变量
cat_vars <- c("gender","hepatitis","cirrhosis","afp200","mvi","vetc","satellite.lesions","degree.of.differentiation")

# 2. 指定非正态分布的连续变量
nonnormal_vars <- c("neut", "lyc", "plt", "mono", "albumin", "tsb", "alt", "ast","alb", 
                    "creatinine", "diameter") 

# 3. 生成基线表（不在 CreateTableOne 里指定 nonnormal）
tab_matched <- CreateTableOne(
  vars = c("age","gender","hepatitis","cirrhosis","afp200","mvi","vetc","satellite.lesions","degree.of.differentiation","neut", "lyc", "plt", "mono",  "tsb", "alt", "ast","alb", 
           "creatinine", "diameter"),
  strata = "ITF",
  data =aa,
  factorVars = cat_vars  # 仅指定分类变量
  ,smd = TRUE
)

# 4. 打印表格时指定非正态变量
tab_matched_df <- as.data.frame(print(tab_matched, 
                                      showAllLevels = TRUE,
                                      nonnormal = nonnormal_vars,  # 在这里指定非正态变量
                                      printToggle = FALSE,
                                      smd = TRUE))
# 5. 添加变量名作为新列
tab_export <- data.frame(Variable = rownames(tab_matched_df),
                         tab_matched_df,
                         row.names = NULL)  # 移除行名
print(matched_data)
# 6. 导出Excel
write.xlsx(tab_export, "C:/Users/86178/Desktop/baseline_table_full.xlsx")

#######进行PSM#####
library(readxl)
library(MatchIt)
library(tableone)
library(cobalt)
psm_model <- matchit(ITF~age+mvi+satellite.lesions+mvi+lyc+degree.of.differentiation+diameter
                     ,  # 包含所有相关变量
                     data = aa,
                     method = "nearest",   # 最近邻匹配
                     distance = "glm",     # 逻辑回归计算倾向评分
                     ratio = 1,            # 1:1匹配
                     caliper = 0.2,        # 卡钳值（标准化后的PS距离）
                     replace = FALSE       # 不允许重复匹配
)
# 查看匹配结果摘要
summary(psm_model)
matched_data <- match.data(psm_model)





















#####IPTW######
#####IPTW后绘制生存曲线#####
library(foreign)
library(RISCA)
library(survminer)
aa<-read_excel("D:/ITF预测侵袭性复发/肝癌复发数据集/合并训练集.xlsx")
categorical_cols <- c("gender", "hepatitis", "cirrhosis", "afp200","degree.of.differentiation","mvi","satellite.lesions","vetc","ITF","diameter1")  # 替换为你的分类变量列名
aa[categorical_cols] <- lapply(aa[categorical_cols], as.factor)
pr<- glm(ITF~gender+hepatitis+cirrhosis+afp200+degree.of.differentiation+mvi+satellite.lesions+vetc+diameter+neut+lyc+plt+mono+alb+tsb+alt+ast+creatinine+age+inr, data=aa, family=binomial(link = "logit"))
pr1<-pr$fitted.values
W <- (aa$ITF==1) * (1/pr1) + (aa$ITF==0) * (1)/(1-pr1)
fit.IPTW=coxph(Surv(time,status==1) ~ ITF+gender+hepatitis+cirrhosis+afp200+degree.of.differentiation+mvi+satellite.lesions+vetc+diameter+neut+lyc+plt+mono+alb+tsb+alt+ast+creatinine+age+inr,
               data=aa,weights=W)
summary(fit.IPTW)
fit2.ipw <- survfit(Surv(time,status1==1) ~ ITF, data = aa,weights = W)
fit3.ipw <- survfit(Surv(OS,status2==1) ~ ITF, data = aa,weights = W)
ggsurvplot(fit2.ipw, data = aa)
library(survminer)
# 提取加权生存曲线的Log-Rank p值
p_value <- surv_pvalue(fit2.ipw, method = "survdiff")$pval
print(paste("Weighted Log-Rank p-value:", p_value))

######IPTW制作基线表#####
library(tableone)
library(survey)
vars=c("gender","hepatitis","cirrhosis","afp200","degree.of.differentiation","mvi","satellite.lesions","vetc","diameter","neut","lyc","plt","mono","alb","tsb","alt","ast","creatinine","age","inr")
psModel=glm(ITF~gender+hepatitis+cirrhosis+afp200+degree.of.differentiation+mvi+satellite.lesions+vetc+diameter+neut+lyc+plt+mono+alb+tsb+alt+ast+creatinine+age+inr,
            family=binomial(link="logit"),
            data=aa)
aa$ps=predict(psModel,type="response")
aa$wt1=1/aa$ps
aa$wt0=1/(1-aa$ps)
aa$w<-ifelse(aa$ITF=="1",aa$wt1,aa$wt0)
dataIPTW=svydesign(ids=~1,data=aa,weights= ~w)
# 修正后的代码
myVars <- c("gender", "hepatitis", "cirrhosis", "afp200", "degree.of.differentiation", 
            "mvi", "satellite.lesions", "vetc", "diameter", "neut", "lyc", "plt", 
            "mono", "alb", "tsb", "alt", "ast", "creatinine", "age", "inr")

# 分类变量
catVars <- c("gender", "hepatitis", "cirrhosis", "afp200",
             "degree.of.differentiation", "mvi", "satellite.lesions", "vetc")

# 非正态连续变量（不包括age，因为它是正态变量）
nonvar <- c("diameter", "neut", "lyc", "plt", "tsb", "alb", "alt", "ast", 
            "creatinine", "mono", "inr")

# 创建平衡表
tab_IPTW <- svyCreateTableOne(
  vars = myVars,
  factorVars = catVars,
  strata = "ITF",
  data = dataIPTW,
  smd = TRUE
)

# 提取完整结果（包含SMD）
Table6 <- print(tab_IPTW,
                nonnormal = nonvar,  # 只包含非正态变量
                catDigits = 1,
                contDigits = 2,
                pDigits = 3,
                showAllLevels = TRUE,
                quote = FALSE,
                noSpaces = TRUE,
                printToggle = FALSE,
                smd = TRUE)

# 导出为Excel表格
library(openxlsx)

# 将Table6转换为数据框，并添加变量名列
Table6_df <- as.data.frame(Table6)
Table6_df$Variable <- rownames(Table6_df)

# 重新排列列顺序，将变量名放在第一列
Table6_df <- Table6_df[, c("Variable", setdiff(names(Table6_df), "Variable"))]

# 导出到Excel
write.xlsx(Table6_df, "D:/ITF预测侵袭性复发/肝癌复发数据集/iptwbase.xlsx")
