library(Hmisc)
library(ggplot2)

##formula

DV ~ IV #Влияет одна переменная
DV ~ IV1 + IV2 #Влияние двух компонент
DV ~ IV1:IV2 # Взаимодействие двух компонет
DV ~ IV1 + IV2 + IV1:IV2 # Влияние двух компонет + взаимодействие
DV ~ IV1 * IV2 # То же самое, что и сверху
DV ~ (IV1 + IV2 + IV3)^2 # Влияние всех компонент и все их взаимодействия до второго уровня

# reading data
df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/shops.csv'))

#ANOVA one-way

boxplot(price ~ origin, data = df)
ggplot(df, aes(df$origin, df$price))+
  geom_boxplot()
fit <- aov(df$price ~ df$origin)
summary(fit)

# Two-way

fit <- aov(df$price ~ df$origin + df$store)
summary(fit)
model.tables(fit,'mean') # срденее по группам 

# Анализ взаимодействия и графики

ggplot(df, aes(origin, price, col = store, group = store))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar',
               position = position_dodge(0.2), width = 0.2, lwd = 0.8)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point',
               position = position_dodge(0.2), size = 2)+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', 
               position = position_dodge(0.2))+
  theme_bw()

#Исходя из графика делаем вывод, что цена варьируется в минимаркетах

fit3 <- aov(df$price ~ df$origin * df$store)
summary(fit3)

# Множественное сравнение с поправкой Тьюки

ggplot(df, aes(food, price))+
  geom_boxplot()

fit4 <- aov(df$price ~ df$food) 
summary(fit4)

TukeyHSD(fit4)

df_th <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/therapy_data.csv'))
str(df_th)

df_th$subject <- factor(df_th$subject)
fit5 <- aov(df_th$well_being ~ df_th$therapy)
summary(fit5)
# А теперь добавим фактор ошибки, связанный с испытуемым, 
#так как над одим исп. проводилось 3 вида психотерапии
fit5b <- aov(df_th$well_being ~ df_th$therapy + Error(df_th$subject/df_th$therapy))
summary(fit5b)

ggplot(df_th, aes(price, well_being))+
  geom_boxplot()+
  facet_grid(~subject)
  
fit5c <- aov(df_th$well_being ~ df_th$therapy * df_th$price + Error(df_th$subject/(df_th$therapy*df_th$price)))
summary(fit5c)

fit6 <- aov(df_th$well_being ~ df_th$price * df_th$sex * df_th$therapy)
summary(fit6)

# задание 5

df3 <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))

df3$patient <- as.factor(df3$patient)
fit1z <- aov(df3$temperature ~ df3$pill + Error(df3$patient/df3$pill))
summary(fit1z)

# Задание 6
fit2z <- aov(df3$temperature ~ df3$doctor * df3$pill + Error(df3$patient/(df3$pill * df3$doctor)))
summary(fit2z)

ggplot(df3, aes(df3$pill, df3$temperature))+
  geom_boxplot()+
  facet_grid(df3$doctor)

# Задание 7

df4 <- ToothGrowth
ggplot(df4, aes(as.factor(dose), len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, 
               position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', 
               position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', 
               position = position_dodge(0.2))




