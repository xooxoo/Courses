library(ggplot2)
library(lazyeval)
library(dplyr)

?apply
data("diamonds")
apply(array, margin, ...)

d <- matrix(rnorm(30), 5)

# В качестве аргумента применяем матрицу d, 1 - применяем
# функцию по строчкам, 2 - по столбцам
apply(d, MARGIN = 1, FUN = sd)
apply(mtcars, 2, sd)

# Задание 1
# В переменной my_df сохранен dataframe с произвольным числом количественных переменных.
# При помощи функции apply найдите максимальное значение в каждой строке. 
# Сохраните результат (вектор максимальных значений) в переменную row_max.

row_max <- apply(my_df, 1, max)

#Напишем функцию с применением apply

outler_count <- function(x){
  outler <- x[abs(x - mean(x)) > 2 * sd(x)]
  if(length(outler) > 0){
    return(outler)
  } else {
    return('No outlier')
  }
}

iris_num <- iris[, 1:4]

apply(iris_num, 2, outler_count)

#! Как мы видим, в данных есть NA
data("airquality")
head(airquality)
apply(airquality, 2, mean)
apply(airquality, 2, mean, na.rm = T)
colMeans(airquality, na.rm = T)
colSums(airquality, na.rm = T)
rowMeans() # для средних в матрице или датафрейме лучше использовать эти функции
rowSums()


set.seed(42) # закрепляет генератор случайных чисел в определенном положении
df <- as.data.frame(matrix(rnorm(30), 5))
my_fun <- function(x)  x * 2
my_fun(1:10)

# Предположим, что хотим выбрать только отрицательные значение наших данных из df 

my_list <- list()

for (i in seq_along(df)){
  temp_col <- df[, i]
  negative_num <- temp_col[temp_col < 0]
  my_list[[i]] <- negative_num
}
my_list
names(my_list) <- colnames(df)

apply(df, 2, function(x) x[x < 0]) # переписали через перегрузку функции 

test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
test_data <- as.data.frame(list(V1 = c(-9.4, -10.8, NA, -10.4, -11.8, NA, NA, NA, NA), V2 = c(11.5, 9.8, 9.8, 12.3, NA, NA, NA, NA, NA), V3 = c(-10.8, -10.2, -9.9, -12.5, NA, NA, NA, NA, NA), V4 = c(-9.1, -12.3, -10.4, -12.3, -9.8, NA, NA, NA, NA), V5 = c(-8.4, -9.3, -10, -10.1, -11.7, NA, NA, NA, NA), V6 = c(-9.8, -9.7, -9.8, -9.5, -9.8, NA, NA, NA, NA), V7 = c(NA, NA, -9.8, -10.9, NA, NA, NA, NA, NA), V8 = c(-9.6, -10, -9.6, -10.6, -11.9, NA, NA, NA, NA), V9 = c(-9.2, -8.8, -12.8, -10.2, -11, NA, NA, NA, NA)))
test_data <- as.data.frame(list(V1 = c(NA, 0.5, 0.7, 8), V2 = c(-0.3, NA, 2, 1.2), V3 = c(2, -1, -5, -1.2)))


# Задание 2
get_negative_values <- function(test_data){
  first <- apply(test_data, 2, function(x) x[x < 0 & !is.na(x)])
  return(first[sapply(first, length) > 0])
}


# Задание 3

test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm  <- function(test_data){
  test_data <- apply(test_data, 2, function(x) {x[is.na(x)] <-  mean(x,na.rm = T)
  return(x)})
  return(as.data.frame(test_data))
}

# Второй вариант

na_rm  <- function(x){    
  na_to_mean  <- function(v){    
    v[is.na(v)]  <- mean(v , na.rm = T)    
    return(v)}    
  result  <- as.data.frame(apply(x, 2, na_to_mean))}

# lapply, sapply, tapply

my_list <- list(x = rnorm(30), y = rnorm(10))
lapply(my_list, mean, na.rm = T) # всегда возвращает список
sapply(my_list, mean) # упрощает вывод

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
positive_sum <- function(x){
  lapply(d, function(col) sum(col[col > 0], na.rm = T))
} 

cars <- c('Mazda', 'Volga', 'Mercedes')
car <- 'Mazda RX4'
grepl('uuu', car)

sapply(cars, function(x) grepl(x, car))

# Задание 4

test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

my_names <- function (dataset, names){
  names <- paste(names, collapse = '|')
  return(dataset[grepl(names, test_data$name),])
}
my_names(test_data, names)

# Задание 5

test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")

fname <- paste(names(test_data[sapply(test_data, function(x) !is.numeric(x))]), collapse = '+')
get(names(test_data[sapply(test_data, function(x) !is.numeric(x))]), test_data)
aggregate(x ~ interp(~fname, fname = paste(names(test_data[sapply(test_data, function(x) !is.numeric(x))]), collapse = '+')), test_data, mean)
?interp
interp(~ fname, fname = paste(names(test_data[sapply(test_data, function(x) !is.numeric(x))]), collapse = '+'))

# Задание 6

test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)

smart_lm <- function(x){
  numdata <- x[sapply(x, is.numeric)]
  shapiro <- sapply(numdata[-1], function(x) shapiro.test(x)$p.value)
  if(length(shapiro[shapiro > 0.05]) == 0){
    return("There are no normal variables in the data")
  }
  else {
    mdata <- numdata[-1][shapiro > 0.05]
    mdata$x <- numdata[,1]
    return(lm(x ~ ., mdata)$coef)
  }
}

# Второй вариант

smart_lm <- function(x){    
  check_norm <- sapply(x[-1], function(var) shapiro.test(var)$p.value > 0.05)    
  if (any(check_norm)){    
    x = x[, c(1, (which(check_norm) + 1))]    
    coef <- lm(x[[1]] ~ ., x[-1])$coef    
    return(coef)    
  } else{    
    return('There are no normal variables in the data')}}
smart_lm(swiss)


# Задание 7 

test_data <- iris[,1:4]


t <- t.test(test_data[,1], mu = 4)
l <- lapply(test_data, function(x) c(t.test(x, mu = 4)$statistic, t.test(x, mu = 4)$parameter,
                                     t.test(x, mu = 4)$p.val))

one_sample_t <- function(test_data, general_mean){
  test_data <- test_data[sapply(test_data, is.numeric)]
  func_list <- lapply(test_data, function(x) c(t.test(x, mu = general_mean)$statistic, 
                                               t.test(x, mu = general_mean)$parameter,
                                               t.test(x, mu = general_mean)$p.val))
  return(func_list)
}


# Задание 8

normality_tests <- lapply(iris[, 1:4], shapiro.test)


get_p_value <- function(test_list){
  return(lapply(test_list, function(x) x$p.val))
}

# Хитрый прием


get_p_value = function(test_data){    
  sapply(test_data, '[', 2)}
# Скобка в кавычках - это индексация, а аргумент после  - это индекс

get_p_value(normality_tests)
