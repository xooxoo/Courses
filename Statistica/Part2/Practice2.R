# Задание 2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], function(col) col - mean(col))
  return(test_data)
}

# Задание 3

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")


fit <- glm(is_prohibited ~ ., test_data1, family = 'binomial')
get_features <- function(dataset){
  fit <- glm(is_prohibited ~ ., dataset, family = 'binomial')
  anova_test <- anova(fit, test = 'Chisq')
  pr_names <- rownames(anova_test[anova_test$`Pr(>Chi)` < 0.05,])[-1]
  if(length(pr_names) == 0){
    return('Prediction makes no sense')
  } else{
    return(pr_names)
  }
}


# Задание 4

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited ~ ., test_data, family = 'binomial')
  data_for_predict$predicted <- predict.glm(fit, data_for_predict, type = 'response')
  return(as.vector(data_for_predict$passangers[which.max(data_for_predict$predicted)]))
}


# Задание 5

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

normality_test <- function(dataset){
  dataset <- dataset[sapply(dataset, is.numeric)]
  return(sapply(dataset, function(col) shapiro.test(col)$p.value))
}
normality_test(test_data)

# Задание 6

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")

smart_anova <- function(test_data){
  shapiro_frame <- data.frame(aggregate(x ~ y, test_data, function(col) shapiro.test(col)$p.val))
  bar_test <- bartlett.test(x ~ y, test_data)$p.val
  if(all(shapiro_frame$x > 0.05) && bar_test > 0.05){
    vec <- summary(aov(x ~ y, test_data))[[1]]$'Pr(>F)'[1]
    names(vec) <- 'ANOVA'
    return(vec)
  } else {
    vec <- kruskal.test(x ~ y, test_data)$p.value
    names(vec) <- 'KW'
    return(vec)
  }
}

smart_anova(test_data)


# Задание 7

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
normality_by <- function(test_data){
  df <- aggregate(x ~ y+z, test_data,function(col) shapiro.test(col)$p.val)
  colnames(df)[3] <- 'p.value'
  return(df)
}
normality_by(test_data)

# Задание 8

library(ggplot2)
iris
ggplot(iris, aes(Sepal.Length, fill =Species))+
  geom_density(alpha = 0.2)
