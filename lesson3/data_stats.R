library(readr)
library(tidyverse)
d1 <- read_csv(file = "E:/R simple tutorial/lesson3/diamonds.csv")
d1
head(d1) #显示前6行
tail(d1) #显示最后6行
#选取或删除
d1 %>% select(5) #选取第5列
d1 %>% select(-5) #删除第5列
d1 %>% select(cut) #选取cut列
d1 %>% select(-cut) #删除cut列
d1 %>% select(c(1,3:5)) #选取第1,3,4,5列
d1 %>% select(c(carat, color:depth)) #选取carat列和color到depth列
d1 %>% pull(cut) #提取cut列为向量

d1 %>% slice(1:5) #选取前5行
d1 %>% slice(-5) #删除第5行
d1 %>% filter(price < 1000) #筛选price小于1000的行
d1 %>% filter(clarity == "VVS2") # 仅筛选VVS2级净度的钻石
d1 %>% slice_sample(n = 5) #随机抽取5行
d1 %>% slice_headtail(n = 3) #选取前3行和后3行
d1 %>% distinct(cut) #显示cut列的不同取值
d1 %>% distinct(cut, color) #显示cut和color列的不同取值组合

# 更改或新建
d1 %>% rename(Clarity = clarity) #将clarity列重命名为Clarity
d1 %>% mutate(Volume = x * y * z) #新建Volume列，值为x*y*z
# 新建变量 price_rmb，即换算为人民币，假设汇率为7
d1 %>% mutate(price_rmb = price * 7)
d1 %>% fill(cut) #用前一行的cut值填充cut列中的缺失值
d1 %>% replace_na(list(cut = "Ideal")) #将cut列中的缺失值替换为"Ideal"
d1 %>% drop_na() #删除含有缺失值的行
d1 %>% rownames_to_column(var = "ID") #将行名转换为ID列
d1 %>% add_count(cut) #按cut分组计数，并将计数结果添加为n列
d1 %>% add_count(cut, name = "cut_count") #按cut分组计数，并将计数结果添加为cut_count列
d2<- d1 %>% unite("cut_color", cut, color, sep = "_") #将cut和color列合并为cut_color列，使用"_"分隔
d2 %>% separate(cut_color, into = c("cut", "color"), sep = "_") #将cut_color列拆分为cut和color列，使用"_"分隔
d1 %>% expand(cut, color) #生成cut和color列的所有可能组合
d1 %>% complete(cut, color) #生成cut和color列的所有可能组合，并保留原数据中的行

#统计
d1 %>% count(cut) #按cut分组计数
d1 %>% summarise(mean_price = mean(price)) #计算price列的均值
d1 %>% summarise(median_price = median(price)) #计算price列的中位数
d1 %>% summarise(sd_price = sd(price)) #计算price列的标准差
d1 %>% summarise(var_price = var(price)) #计算price列的方差
d1 %>% summarise(min_price = min(price)) #计算price列的最小值
d1 %>% summarise(max_price = max(price)) #计算price列的最大值
d1 %>% summarise(range_price = max(price) - min(price)) #计算price列的极差

#分组
d1 %>% summarise(mean_price = mean(price), .by = clarity) #按clarity分组，计算price列的均值
d1 %>% group_by(cut) %>% summarise(mean_price = mean(price)) #按cut分组，计算price列的均值
d1 %>% group_by(cut, color) %>% summarise(mean_price = mean(price)) #按cut和color分组，计算price列的均值
d1 %>%
  mutate(
    price = mean(price),
    .by = clarity
  )
#按clarity分组，计算price列的均值，并将结果赋值给price列
d1 %>%
  mutate(
    mean_price = mean(price),
    .by = clarity
  ) #按clarity分组，计算price列的均值，并将结果赋值给新建的mean_price列
d1 %>%
  mutate(
    price3g = cut(price, 3),
    .by = color
  ) #按color分组，将price列分为3组，并将结果赋值给新建的price3g列
d1 %>% group_by(cut) #按cut分组
d1 %>% group_by(cut) %>% summarise(price =mean(price)) #按cut分组，计算price列的均值
d1 %>% group_by(cut) %>% filter(price == max(price)) #按cut分组，筛选price列的最大值所在的行

#合并
d3 <- d1 %>% select(1:6) %>% slice(1:10) #选取前10行和前6列，赋值给d3
d31<- d3 %>% slice(1:5) #显示d3的前5行
d32<- d3 %>% slice(6:10) #显示d3的第6到10行
d31
d32
bind_rows(d31, d32) #按行合并d31和d32

d33 <- d3 %>% select(c(1:3)) #选取d3的第1,2,3列，赋值给d33
d34 <- d3 %>% select(c(4:6)) #选取d3的第4,5,6列，赋值给d34
bind_cols(d33, d34) #按列合并d33和d34
d33 %>% left_join(d34) #左连接d33和d34
d34 <- d3 %>% select(c(1,4:6)) #选取d3的第1,4,5列，赋值给d34
d33
d34
d33 %>% left_join(d34) #左连接d33和d34
d33 %>% left_join(d34, by = "...1") #指定连接键，左连接d33和d34
d33 %>% right_join(d34) #右连接d33和d34

#习题：解释以下函数
d1 %>% filter(cut == "Ideal" & price < 1000) 

d1 %>% group_by(cut) %>% summarize(mean_price = mean(price))

d1 %>% group_by(cut) %>% mutate(mean_price = mean(price))

# 排序，要求按照price从大往小排序，但希望cut为Ideal的排在最前面

                                        