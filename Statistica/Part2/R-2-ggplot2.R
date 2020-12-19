library(ggplot2)
library(dplyr)
data('diamonds')

qplot(x = price, data = diamonds)
qplot(x = price, y = carat, data = diamonds)
qplot(x = cut, y = carat, data = diamonds)

qplot(x = price,
      y = carat,
      color = color,
      facets = ~cut,
      alpha = clarity,
      data = diamonds,
      geom = 'point')
qplot(x = price, col= cut, data = diamonds, geom = 'density', alpha = I(0.2))


ggplot(diamonds, aes(price, fill = cut))+
  geom_histogram()
ggplot(diamonds, aes(price, carat))+
  geom_point(size = 0.5, alpha = .2, aes(col = cut)) +
  geom_smooth()+
  facet_wrap(~clarity)

gr_airquality <- group_by(airquality, by = Month)
t <- summarise_all(gr_airquality, mean, na.rm = T)

ggplot(t, aes(Month, Temp))+
  geom_line()+
  geom_point(aes(size = Wind, col = Solar.R))+
  geom_hline(yintercept = 75, linetype = 'dotted')

# Задание 1
data('mtcars')
mtcars
my_plot <- ggplot(mtcars, aes(factor(am), mpg))+
  geom_violin()+
  geom_boxplot(width = 0.2)
my_plot

# Задание 2

sales <- read.csv('sales.csv')
my_plot <- ggplot(sales, aes(income, sale))+
  geom_point(aes(col = shop))+
  geom_smooth()

# Задание 3

my_plot <- ggplot(sales, aes(shop, income, col = season))+
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))
my_plot

# Задание 4

my_plot <- ggplot(sales, aes(date, sale, col = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, position = position_dodge(0.2))+
  stat_summary(fun.y = mean, geom = 'point', position = position_dodge(0.2))+
  stat_summary(fun.y = mean, geom = 'line', position = position_dodge(0.2))
my_plot
