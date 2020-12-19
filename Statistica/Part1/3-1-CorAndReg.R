library(ggplot2)
library(psych)


fit <- cor.test(df$mpg, df$hp)
cor.test(~mpg + hp, df)
plot(df$mpg, df$drat)
ggplot(df, aes(mpg, hp))+
  geom_point(aes(col = cyl))
df <- df[, c(1, 3:7)]
pairs(df)
cor(df)

fit <- corr.test(df, adjust = 'bonferroni')
df[2]

# задание 1

corr.calc <- function(x){
  corr.test(x[1], x[2])
  return(c(corr.test(x[1], x[2])$r, corr.test(x[1], x[2])$p))
}
corr.calc(df[,c(1,2)])

# Задание 2

df <- read.table(url('https://stepic.org/media/attachments/lesson/11504/step6.csv'), header = T, sep = ',')
filtered.cor <- function(x){
  x <- x[,sapply(x, function(x) is.numeric(x))] 
  ff <- corr.test(x)$r
  diag(ff) <- 0
  return(ff[which.max(abs(ff))])
}
filtered.cor(df)
shapiro.test(df)$p
hist(df)
# Задание 3

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor <- function(x) {
  if (shapiro.test(x[,1])$p <= 0.05 | shapiro.test(x[,2])$p <= 0.05 ) {
    return(corr.test(x[1], x[2], method = 'spearman')$r)
  }
  else {
    return(corr.test(x[1], x[2])$r)
  }
}
smart_cor(test_data)
test_data[1]
f <- cor.test(test_data[,1], test_data[,2])
f$estimate


# Сделаем простую линейную модель

df <- mtcars
df_num <- df[, c(1, 3:7)]
fit <- lm(mpg~ hp, df)
summary(fit)
ggplot(df, aes(hp, mpg ))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  facet_grid(.~cyl)
# Вызываем значения, построенные по нашей модели
fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)
new_hp <- data.frame(hp = c(100, 150, 129, 300))
# А теперь сделаем прогноз на пустые значения
predict(fit, new_hp)
new_hp$mpg <- predict(fit, new_hp)

# Линейная регрессия и номинативные данные
my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c('four', 'six', 'eigth'))

fit <- lm(mpg~cyl, my_df)
fit$fitted.values
summary(fit)
# Как мы видим, при построении модели за b0 берется среднее значение одного из уровней 
# переменной cyl, а все остальные коэф. показывают ищменение при переходе на другую градацию
ggplot(my_df, aes(cyl, mpg))+
  geom_point(aes(col = factor(am, labels = c('manual', 'auto'))))+
  theme(axis.text = element_text(family = 'ubuntu', size = 10),
        axis.title = element_text(size = 15, family = 'ubuntu'),
        legend.title = element_text(family = 'ubuntu', size = 10))+
  scale_colour_manual(name = 'tr',
                    values = c("#c23b22", "#E69F00"))
  
aggregate(mpg~cyl, my_df, mean)

# Задание 6

my_df <- iris[, c(1,4)]

regr.calc <- function(x){
  if (corr.test(x[1], x[2])$p < 0.05){
    x$fit <- lm(x[,1] ~ x[,2], x)$fitted.values
    return(x)
  } 
  else {
    return('There is no sence in prediction')
  }
}

my_df <- regr.calc(my_df)
corr.test(my_df$Sepal.Width, my_df$Sepal.Length)
ggplot(my_df, aes(my_df$Petal.Width, my_df$Sepal.Length))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_point(aes(my_df$Petal.Width, my_df$fit, col = 'red'))

# Задание 7

ggplot(iris, aes(Sepal.Width, Petal.Width,col = Species))+
  geom_point()+
  geom_smooth(method = 'lm')

