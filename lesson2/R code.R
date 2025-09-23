library(tidyverse)
library(dplyr)
d <- read.csv(file = "E:/R simple tutorial/Lesson2/diamonds.csv")
d <- tibble::as_tibble(d)
# 展示数据的方式
head(d)
d
View(d)
# 接下来 我会展示一些排序的示例，请特别关注列的顺序变化
relocate(d, price) # 将price列移到最前面
relocate(diamonds, price, .before = cut) # 把price提到cut之前，其余顺序不变
relocate(diamonds, price, .after = cut) # 把price提到cut之后，其余顺序不变

#升序和降序
arrange(d, price) # 按price升序排列
arrange(d, desc(price)) # 按price降序排列

# magrittr 管道 %>%
# 任务：先把 price 放到开头，再对它降序排列（从高到低）
d %>%
  relocate(price) %>%
  arrange(desc(price))

# 选取和修改
d %>%
  select(5) # 选取第五列
d %>%
  select(-5) # 除了第五列，其他都选取
d %>%
  select(price, carat) # 选取price和carat两列
d %>%
  select(-price, -carat) # 除了price和carat，其他都选取

# : 和 c() 的用法
1:5
d %>%
  select(1:5) # 选取前五列
d %>%
  select(carat:depth) # 选取从carat到price的所有列
c(1,3:5)
d %>%
  select(c(1,3:5)) # 选取第1、3、4、5列
d %>%
  select(c(carat, color:depth))
# 选取carat列，以及从color到depth的所有列
# starts_with()、ends_with()、matches()、everything()、last_col()
d %>%
  select(starts_with("c")) # 选取所有以c开头的列
d %>%
  select(ends_with("e")) # 选取所有以e结尾的列
d %>%
  select(matches("a")) # 选取所有包含a的列
d %>%
  select(everything())# 选取所有列
d %>%
  select(last_col()) # 选取最后一列
d %>%
  select(last_col(3)) # 选取倒数第三列

# where()的用法
d %>%
  select(where(is.numeric)) # 选取所有数值型列
d %>%
  select(where(is.character)) # 选取所有字符型列
d %>%
  select(where(is.factor)) # 选取所有因子型列
d %>%
  select(where(is.numeric)) %>%
  select(where(~ mean(.) < 8)) # 选取所有数值型列中均值小于8的列

library(dplyr)

d_modified <- d %>%
  select(carat, cut, price, starts_with("c")) %>%
  mutate(price_per_carat = price / carat)
d_modified
