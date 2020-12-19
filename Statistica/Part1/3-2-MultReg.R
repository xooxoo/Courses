library(ggplot2)
library(MASS)

swiss <- data.frame(swiss)
hist(swiss$Fertility)
fit <- lm(Fertility ~ Examination + Catholic, swiss)

# Задание 1
fill_na <- function(x){
  x_sub <- na.exclude(x)
  fit <- lm(y ~ x_1 + x_2, x_sub)
  x$y_full <- predict(fit, x)
  x$y_full[!is.na(x[,3])] <- x[,3][!is.na(x[,3])]
  return(x)
}

# Задание 2

mtcars <- subset(mtcars, select = c('mpg', 'hp', 'drat', 'wt', 'disp'))
model <- lm(wt ~ mpg + disp + hp + drat, mtcars)
stepAIC(model, mtcars)


hist(swiss$Catholic)
swiss$religious <- ifelse(swiss$Catholic > 60, 'High', 'Low')

fit2 <- lm(Fertility ~ religious * Examination, swiss)
summary(fit2)
