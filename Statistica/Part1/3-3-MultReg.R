library(ggplot2)
library(MASS)

swiss <- data.frame(swiss)
hist(swiss$Fertility)
fit <- lm(Fertility ~ Examination + Catholic, swiss)
swiss$religious <- ifelse(swiss$Catholic > 60, 'High', 'Low')

ggplot(swiss, aes(Examination, Fertility, col = religious))+
  geom_point()+
  geom_smooth(method = 'lm')

swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., swiss) # ~ . в формуле означает, что мы хотим использовать все предикторы 
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education + Examination, swiss)
summary(fit_reduced1)

# Чтобы сравнить две модели (полную и укоченную) мы воспользуемся дисперсионным анализом

anova(fit_full, fit_reduced1)

# Автоматический отбор при помощи библитеки MASS
fit <- lm(Fertility ~ .*., swiss)
summary(fit)
stepAIC(fit)

# Автоматический отбор при помощи встроенной библиотеки

optimal <- step(fit, direction = 'backward')
step(fit, direction = 'forward')

summary(optimal)


# Задание 1

att <- data.frame(attitude)

model_full <- lm(rating ~ .*., att)
model_null <- lm(rating ~ 1, att)
# Модель, в которой нет ни одного предиктора, а только intercept,
# т.е. среднее значение переменной
# С помощью аргумента scope мы можем задать пространство моделей с разным числом предикторов,
# в котором будет происходить поиск оптимального набора предикторов

ideal_model <- step(model_full, scope = list(lower = model_null, upper = model_full), 
                    direction = 'backward')

anova(model_full, ideal_model)