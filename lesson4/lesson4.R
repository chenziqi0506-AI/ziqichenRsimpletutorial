library(readr)
library(dplyr)
library(broom)
library(car)
library(caret)
library(modelr)
library(ggpubr)
wages <- read_csv(file = "/Volumes/My Passport/R simple tutorial/lesson4/wages.csv")
wages

# 男性是否比女性赚的多？
#正态性检验（检查earn（收入）在不同性别组（sex）中的分布是否符合正态分布）
wages %>% group_by(sex) %>% summarise(shapiro_p=shapiro.test(earn)$p.value)
#方差齐性检验（检验男性和女性收入的方差是否相等（方差齐性））
leveneTest(earn~sex, data=wages)
#单因素方差分析(原假设（H₀）：男性和女性收入均值相等)
anova_result <- aov(earn~sex, data=wages)
summary(anova_result)
wages %>% group_by(sex) %>% summarise(mean_earn=mean(earn), sd_earn=sd(earn))

#我们采用ggpubr宏包下的ToothGrowth来说明，这个数据集包含60个样本，记录着每10只豚鼠在不同的喂食方法和不同的药物剂量下，牙齿的生长情况.
#len : 牙齿长度
#supp : 两种喂食方法 (橙汁和维生素C)
#dose : 抗坏血酸剂量 (0.5, 1, and 2 mg)
my_data <- ToothGrowth %>% mutate(across(c(supp, dose), ~ as.factor(.x))) #转换为因子变量，因为ANOVA要求自变量为分类变量。
head(my_data)
#问题：豚鼠牙齿的长度是否与药物的食用方法和剂量有关？
#线性回归时，我们是通过独立变量来预测响应变量，但现在我们关注的重点会从预测转向不同组别差异之间的分析，这即为方差分析（ANOVA）。
#这里是两个解释变量，所以问题需要双因素方差分析 (two-way ANOVA)
#检验喂食方法（supp）和剂量（dose）对牙齿长度（len）的主效应，不含交互效应
anova_result2 <- aov(len ~ supp + dose, data = my_data) %>% broom::tidy()
anova_result2
#结果显示，supp（喂食方法）和dose（剂量）对牙齿长度均有显著影响 (p < 0.05)。
#检验表明不同类型之间存在显著差异，但是并没有告诉我们具体谁与谁之间的不同。需要多重比较帮助我们解决这个问题。使用TurkeyHSD函数
aov(len ~ supp + dose, data = my_data) %>%
  TukeyHSD(which = "dose")%>%
  broom::tidy()
aov(len ~ supp + dose, data = my_data) %>%
  TukeyHSD(which = "supp") %>%
  broom::tidy()
#量越高，牙齿长度越长（2 mg > 1 mg > 0.5 mg），所有组间差异均显著
#VC的牙齿长度比OJ短3.70单位，橙汁（OJ）喂食的豚鼠牙齿长度显著长于维生素C（VC）。
#思考：交互效应是否显著？
anova_result3 <- aov(len ~ supp * dose, data = my_data) %>% broom::tidy()
anova_result3
#喂食方法和剂量的组合对牙齿长度有显著影响，即supp的影响依赖于dose的水平（反之亦然）。


#线性回归
wages %>% head()
wages %>% colnames()
wages %>% summarise_all(~sum(is.na(.))) #检查缺失值
wages %>% count(sex) #检查类别变量的水平
wages %>% group_by(sex) %>% summarise(mean_height = mean(height), mean_earn = mean(earn)) #按性别分组计算身高和收入的均值
#长得高的人挣钱越多？
model1 <- lm(earn ~ height, data = wages) #构建线性回归模型，检验身高（height）对收入（earn）的影响。
summary(model1)
#表示身高每增加1英寸，收入平均增加2387单位。模型拟合较低
#多元线性回归
model2 <- lm(earn ~ height + ed, data = wages) #加入教育年限（ed）作为变量
summary(model2)
#大家尝试解读一下model2的结果




#哪个变量对收入的影响最大？
lm(earn ~ height + ed + age, data = wages)
# 方法一，变量都做完标准化后，比较回归系数的绝对值
fit <- wages %>%
  mutate_at(vars(earn, height, ed, age), scale) %>%
  lm(earn ~ 1 + height + ed + age, data = .) 
summary(fit)
# 方法二，通过比较模型参数的t-statistic的绝对值，可以考察参数的重要程度
caret::varImp(fit)
