library(ggplot2)
library(tidyverse)
library(patchwork)
library(readr))
wages <- read_csv(file = "/Volumes/My Passport/R simple tutorial/lesson4/wages.csv")
head(wages)
diamonds <- read_csv(file = "/Volumes/My Passport/R simple tutorial/lesson2/diamonds.csv")
head(diamonds)
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point() #散点图，展示了钻石的克拉数与价格之间的关系
# 1. 散点图：克拉 vs 价格（按切工着色）（alpha是透明度）
ggplot(diamonds, aes(carat, price, color = cut)) + geom_point(alpha = 0.6)
# 2. 柱状图：钻石切工数量
ggplot(diamonds, aes(cut)) + geom_bar(fill = "steelblue")
# 3. 直方图：价格分布
ggplot(diamonds, aes(price)) + geom_histogram(bins = 30, fill = "lightgreen")
# 4. 箱线图：不同切工的价格分布
ggplot(diamonds, aes(cut, price)) + geom_boxplot(fill = "lightpink")
# 5. 密度图：价格密度分布（按切工着色）
ggplot(diamonds, aes(price, fill = cut)) + geom_density(alpha = 0.5)
# 6. 小提琴图：不同切工的价格分布
ggplot(diamonds, aes(cut, price)) + geom_violin(fill = "lightblue")
# 7. 折线图：按克拉分组的平均价格趋势
diamonds_summary <- diamonds %>%
  group_by(carat) %>%
  summarize(avg_price = mean(price))
ggplot(diamonds_summary, aes(carat, avg_price)) + geom_line(color = "purple")
# 8. 点图：按切工分组的平均价格
cut_summary <- diamonds %>%
  group_by(cut) %>%
  summarize(avg_price = mean(price))
ggplot(cut_summary, aes(cut, avg_price)) + geom_point(size = 4, color = "orange")
# 9. 堆积柱状图：按切工和颜色分组的钻石数量
ggplot(diamonds, aes(cut, fill = color)) + geom_bar(position = "stack")
# 10. 分面散点图：克拉 vs 价格（按切工分面）
ggplot(diamonds, aes(carat, price)) + geom_point(alpha = 0.5) + facet_wrap(~ cut)

#公式如下：
ggplot(___) + 
  geom_point(
    mapping = aes(x = ___, y = ___)
  )
#其中，___部分需要根据具体的数据和图形类型进行填写。例如，对于散点图，可以填写数据集名称、x轴变量和y轴变量
#例如：
ggplot(data = wages) + 
  geom_point(
    mapping = aes(x = age, y = earn)) + xlab("Age") + ylab("Earnings") + ggtitle("Scatterplot of Earnings vs. Age")
#这段代码创建了一个散点图，展示了年龄（age）与收入（earn）之间的关系，并添加了轴标签和标题。
#另一种写法：
ggplot(data = wages, mapping = aes(x = age, y = earn)) + 
  geom_point() + xlab("Age") + ylab("Earnings") + ggtitle("Scatterplot of Earnings vs. Age")
#这段代码实现了与前一段代码相同的效果，但将数据和映射直接传递给了ggplot()函数。
#ggplot() 初始化绘图，相当于打开了一张纸，准备画画。
#ggplot(data = wages) 表示使用wages这个数据框来画图。
#+表示添加图层。
#geom_point()表示绘制散点图。
#aes()表示数值和视觉属性之间的映射。
#aes(x = Age, y = Earnings)，意思是变量Age作为（映射为）x轴方向的位置，变量Earnings作为（映射为）y轴方向的位置。
#aes()除了位置上映射，还可以实现色彩、形状或透明度等视觉属性的映射。

#例如，下面的代码将点的颜色映射到变量sex上：
ggplot(data = wages, mapping = aes(x = age, y = earn, color = sex)) + 
  geom_point() + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings grouped by sex")
#让我们试下以下代码
ggplot(data = wages, mapping = aes(x = age, y = earn, shape = sex)) + 
  geom_point() + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings grouped by sex")

ggplot(data = wages, mapping = aes(x = age, y = earn, size = sex)) + 
  geom_point() + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings grouped by sex")

ggplot(data = wages, mapping = aes(x = age, y = earn, alpha = sex)) + 
  geom_point() + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings grouped by sex")

#也可以有更多映射
ggplot(data = wages, mapping = aes(x = age, y = earn, color = sex, shape = race)) + geom_point() + 
  xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings grouped by sex and race")

#绘制平滑曲线
ggplot(data = wages, mapping = aes(x = age, y = earn)) + 
  geom_smooth() + xlab("Age") + ylab("Earnings") + ggtitle("Smoothed Age vs Earnings")
#绘制平滑曲线，并按性别分组
ggplot(data = wages, mapping = aes(x = age, y = earn, color = sex)) +
  geom_smooth() + xlab("Age") + ylab("Earnings") + ggtitle("Smoothed Age vs Earnings")
#同时绘制散点图和平滑曲线
ggplot(wages) + geom_point(mapping = aes(x = age, y = earn)) + 
  geom_smooth(mapping = aes(x = age, y = earn)) + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings with Smoothed Line")
ggplot(wages, mapping = aes(x = age, y = earn)) + 
  geom_point() + geom_smooth() + xlab("Age") + ylab("Earnings") + ggtitle("Age vs Earnings with Smoothed Line")
#保存图片
ggsave("age_earnings_plot.png")
ggsave("age_earnings_plot.png", width = 8, height = 6, dpi = 300)


#练习
#1. 使用wages数据集，绘制年龄（age）与收入（earn）之间的散点图，并按性别（sex）着色。




