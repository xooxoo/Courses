library(ggplot2)
library(ROCR)

my_df <- read.csv('https://stepic.org/media/attachments/lesson/10226/train.csv', sep = ';')

ggplot(my_df, aes(read, math, col = gender))+
  geom_point()+
  facet_grid(.~hon)

fit <- glm(hon ~ read + math + gender, my_df, family = 'binomial')
?glm
summary(fit)
exp(fit$coefficients)

head(predict(fit, type = 'response'))

my_df$propab <- predict(fit, type = 'response')*100

ggplot(my_df, aes(propab, math, col = gender))+
  geom_point()


# Задание 1

cars <- mtcars

log_coef <- glm(am ~ disp + vs + mpg, cars, family = 'binomial')$coef


# Задание 2

obj <- ggplot(ToothGrowth, aes(supp, len, fill = factor(dose)))+
  geom_boxplot()
obj

# Для построения ROCR кривых мы используем пакет ROCR

pred_fit <- prediction(my_df$propab, my_df$hon) # В этот объект помещаются предсказанные знач. и реальные
perf_fit <- performance(pred_fit, 'tpr', 'fpr') # Позволяет  рассчитать значения rate
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0, 100, by = 10))
?performance

auc <- performance(pred_fit, 'auc')
str(auc)


# Чтобы принять решение, где ставить порог отсечения нашей модели, при котором мы принимаем
# положительное решение воспользуемся графиками чувствительности и специфичности

perf3 <- performance(pred_fit, x.measure = 'cutoff', measure = 'spec') # никто не получит диплом
perf4 <- performance(pred_fit, x.measure = 'cutoff', measure = 'sens') # все получат красный диплом
perf5 <- performance(pred_fit, x.measure = 'cutoff', measure = 'acc') # соотношение порога и общей эффективности классификатора

plot(perf3, col = 'red', lwd = 2)
plot(add = T, perf4, col = 'green', lwd = 2)
plot(add = T, perf5, lwd = 2)
legend(x = 10, y = 0.5, c('spec', 'sens', 'accur'),
       lty = 1, col = c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)
abline(v = 22.5)

my_df$prep_resp <- factor(ifelse(my_df$propab > 22.5, 1, 0), labels = c('N', 'Y'))
my_df$correct <- ifelse(my_df$hon == my_df$prep_resp, 1, 0)

# Построим график, который покажет насколько хорошо нам получилось предсказать данные

ggplot(my_df, aes(propab, fill = factor(correct)))+
  geom_dotplot()
mean(my_df$correct) # т.к. у нас всего два уровня (1 и 0), то среднее будет показывать процент удачных исходов

test_df <- read.csv('https://stepic.org/media/attachments/lesson/10226/test.csv', sep = ';')

test_df$hon <- NULL
test_df$hon <- predict(fit, test_df, type = 'response')


#Задание 3 

amer <- read.csv('https://stepic.org/media/attachments/lesson/11478/data.csv')
amer_sub <- na.exclude(amer)
fit_a <- glm(admit ~ rank*gpa, amer_sub, family = 'binomial')
amer$admit_pred <- predict(fit_a, amer, type = 'response')
amer$admit_pred <- ifelse(is.na(amer$admit), 
                          ifelse(amer$admit_pred >= 0.4, 1, 0), 
                          NA)
sum(amer$admit_pred, na.rm = T)


ggplot(amer_sub, aes(gre, gpa, col = factor(admit)))+
  geom_point()+
  facet_grid(.~rank)
