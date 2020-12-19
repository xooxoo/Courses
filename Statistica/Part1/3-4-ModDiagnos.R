library(ggplot2)
library(QuantPsyc)
library(gvlma)

data(swiss)
str(swiss)
pairs(swiss)
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme(axis.text = element_text(family = 'ubuntu', size = 9),
        axis.title = element_text(family = 'ubuntu', size = 12, face = 'bold'))

# Проверим на нормальность распределение наших переменных
ggplot(swiss, aes(Examination))+
  geom_histogram(binwidth = 3)

ggplot(swiss, aes(Education))+
  geom_histogram(binwidth = 3)

# Видно, что переменная Education не распределена нормально. Ее лучше не использовать, либо
# ее необходимо преобразовать. В данном случае возьмом логарифм переменной

ggplot(swiss, aes(log(Education)))+
  geom_histogram()


# Задание 1

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 
               0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 
               0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 
               0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 
               0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 
               0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 
               0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

shapiro.test(1/my_vector)

# Задание 2

beta.coef <- function(x){
  return(lm(scale(x[,1]) ~ scale(x[,2]))$coefficients)
}

lm.beta(lm(mtcars[,1] ~ mtcars[,2])) # делает абсолютно то же самое, что и код наверху

# Задание 3


normality.test <- function(x){
  mm <- sapply(x, function(x) shapiro.test(x))
  mm <- as.data.frame(mm)[2,]
  return(sapply(mm, as.numeric))
}

# хороший вариант решения

normality.test <- function(x){
  return(sapply(x, FUN = shapiro.test)['p.value',])
}


# Проверка линейности взаимосвязи между зависимой и независимой переменной

ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_sq <- swiss$Examination ^ 2

lm2 <- lm(Education ~ Examination + Examination_sq, swiss)
summary(lm2)

anova(lm1, lm2)

swiss$lm1_fitted <- lm1$fitted.values
swiss$lm2_fitted <- lm2$fitted.values
swiss$lm1_res <- lm1$residuals
swiss$lm2_res <- lm2$residuals
swiss$obs_num <- 1:nrow(swiss)

# Построим графики с моделями
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_line(aes(Examination, lm1_fitted, col = 'red'))+
  geom_line(aes(Examination, lm2_fitted, col = 'blue'))

# Теперь построим распределение остатков

ggplot(swiss, aes(lm1_fitted, lm1_res))+
  geom_point()+
  geom_hline(aes(yintercept = 0, col = 'red'))

ggplot(swiss, aes(lm2_fitted, lm2_res))+
  geom_point()+
  geom_hline(aes(yintercept = 0, col = 'red'))

# Проверка на независимость остатков
# Для этого построим простой график, где по оси X идут номера налюдений, а по Y  идут остатки моделей
# Как видно, никаких группировок остаков не обнаружено, но в идеале линия должна быть прямой 

ggplot(swiss, aes(obs_num, lm1_res))+
  geom_point()+
  geom_smooth()

ggplot(swiss, aes(obs_num, lm2_res))+
  geom_point()+
  geom_smooth()

# Проверка на гомоскедастичность. Это требование к моделируемым данным, согласно которому изменчивость
# остатков должна быть одинковой на протяжении всего моделируемого вектора

ggplot(swiss, aes(lm1_fitted, lm1_res))+
  geom_point()

ggplot(swiss, aes(lm2_fitted, lm2_res))+
  geom_point()

# Задание 4
# Функция gvlma похволяет получить оценку выполнения основных допущений линейной регресии. В качестве
# аргумента она принимает модель. 

homosc <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
x <- gvlma(DV ~ IV, homosc)
summary(x)

# Нормальность распределения остатков 

ggplot(swiss, aes(lm1_res))+
  geom_histogram()

qqnorm(swiss$lm1_res)
qqline(swiss$lm1_res)
shapiro.test(swiss$lm1_res)

ggplot(swiss, aes(lm2_res))+
  geom_histogram()

qqnorm(swiss$lm2_res)
qqline(swiss$lm2_res)
shapiro.test(swiss$lm2_res)

#Задание 5

resid.norm <- function(x){
  if (shapiro.test(x$residuals)$p < 0.05){
    my_plot <- ggplot(x, aes(x$residuals))+
      geom_histogram(col = 'red', fill = 'red')
    return(my_plot)
  } else {
    my_plot <- ggplot(x, aes(x$residuals))+
      geom_histogram(col = 'green', fill = 'green')
    return(my_plot)
  }
}

resid.norm(lm1)
# Более красивый вариант

resid.norm <- function(x){
  pval <- shapiro.test(x$residuals)$p
  plt <- ggplot(as.data.frame(x$model), aes(x$residuals))+
    geom_histogram(fill = ifelse(pval < 0.05, 'red', 'green'))
  return(plt)
}

# Задание 6

high.corr <- function(x) {
  cor_mat <- cor(x)
  diag(cor_mat) <- NA
  logic_vec <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, abs(cor_mat),max(abs(cor_mat), na.rm = T))
  return(rownames(which(cor_mat == cor_mat[logic_vec], arr.ind = T)))
}

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- NA
  return(rownames(which(abs(cr)==max(abs(cr), na.rm = T),arr.ind=T)))
  }

high.corr(swiss)

test_data <- as.data.frame(list(V1 = c(1.5, 0.7, -0.2, -1.1, -0.6, -0.7, 2.3, -0.7, 0.3, 0.1), V2 = c(-1.4, 0.3, 1.3, -0.3, 0.8, -1.2, -1.4, 0.5, -1.5, 0.5), V3 = c(0.2, -0.1, 0.1, -0.4, 0.2, -0.7, -0.5, -0.8, 0.6, -0.9), V4 = c(-0.6, 2, -0.2, 2, 1.1, 0.2, -0.6, 0.2, -1.1, -1), V5 = c(-0.6, 0.1, 1, -0.5, -0.7, -0.5, 0.7, 1.3, 1.5, 0.5), V6 = c(-1.5, -0.7, 0.2, 1.1, 0.6, 0.7, -2.3, 0.7, -0.3, -0.1)))
high.corr(test_data)

crm <- cor(test_data)
diag(crm) <- NA
which(max(abs(crm), na.rm = T))
crm == max(abs(crm), na.rm = T)
l <- which(mapply(function(x, y) {isTRUE(all.equal(x, y))}, abs(crm),max(abs(crm), na.rm = T)), arr.ind = T)
ll <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, abs(crm),max(abs(crm), na.rm = T))
which(crm == crm[ll], arr.ind = T)
