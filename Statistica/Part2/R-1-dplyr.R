library(dplyr)
library(ggplot2)
library(lazyeval)

my_data <- data_frame(x = rnorm(10000), y = rnorm(10000),
                      f = factor(rep(1:2, 5000)))
my.data <- data.frame(x = rnorm(10000), y = rnorm(10000),
                      f = factor(rep(1:2, 5000)))
my.data
my_data

diamonds <- as_data_frame(diamonds)

my_data_2 <- data_frame('My data' = rnorm(1000))
my.data.2 <- data.frame('My data' = rnorm(1000))
my_data_2$`My data`
my.data.2$My.data

# позволяет сразу объявлять переменную
my_data_2 <- data_frame(x = rnorm(10), y = abs(x)) 

diamonds <- as_data_frame(diamonds)

select(diamonds, cut, price)
select(diamonds, cut:price)

mutate(mtcars, am = factor(am))

mutate_each(my_data_2, funs(as.factor))

# Задание 1
test_data <- as.data.frame(list(V1 = c(1.7, -0.7, -0.3, -0.9, 0.7), V2 = c(-0.1, 0, -0.1, -0.3, -0.6), V3 = c("B", "B", "B", "A", "B"), V4 = c(-1.5, 0.5, 0.6, 0.7, 1.2)))

log_transform <- function(test_data){
  test_data <- mutate_if(test_data, is.numeric, funs((. - min(.))/(max(.) - min(.)) + 1)) %>% 
    mutate_if(is.numeric, funs(log))
  return(test_data)
}

diam <- group_by(diamonds, cut, color)
sample_n(group_by(diamonds, cut), 2)
summarise(diam, 
          observ = n(), 
          m_price = mean(price), 
          x = mean(x), 
          great_price = sum(price > 5000))
summarise_all(diam, mean)
?summarise_each()

sum(filter(diam, cut == 'Fair', color == 'D')$price > 5000)
summarise_at(diam)

# Задание 2

test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")

descriptive_stats <- function (dataset){
  test_data <- group_by_if(test_data, is.factor) %>% 
    summarise(n = n(),
              mean = mean(salary, na.rm = T),
              sd = sd(salary, na.rm = T),
              median = median(salary, na.rm = T),
              first_quartile = quantile(salary, 0.25, na.rm = T),
              third_quartile = quantile(salary, 0.75, na.rm = T),
              na_values = sum(is.na(salary))
    )
  return(test_data)
}

# Задание 3

test_data <- mtcars

to_factors <- function(dataset, cols){
  return(dataset %>% 
  mutate_at(cols, funs(as.factor(ifelse(mean(.) > ., 1, 0)))))
}

# Задание 4

test_data <- group_by(diamonds, color)

test_data <- test_data %>% 
  arrange(desc(price)) %>% 
  slice(1:10) %>% 
  select(color, price)

# Задание 5

test_data <- ToothGrowth %>% 
  mutate(dose = as.factor(dose))
find_outlier <- function(t){
  name <- names(which(sapply(t, is.numeric)))
  return(t <- t %>% 
    group_by_if(is.factor) %>%
    mutate_(is_outlier = interp(~(ifelse(abs(var - mean(var)) > 2 * sd(var), 1, 0)), var = as.name(name))))
}


summarise(test_data, 2 * sd(len))
test_data
t <- find_outlier(test_data)
t
