library(compare)
library(ggplot2)

# Вторая задача

smart_test <- function(x){
 if (any(table(x) < 5)){
   return(fisher.test(table(x))$p)
 } else {
   return(c(chisq.test(table(x))$statistic[[1]],
            chisq.test(table(x))$parameter[[1]],
            chisq.test(table(x))$p.value))
 }
}


# Третья задача

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

most_significant <- function(x){
  df <- as.data.frame(sapply(x, function(x) table(x)))
  p_vec <- vector()
  for(i in 1:length(names(df))){
    p_vec <- append(p_vec,chisq.test(df[,i])$p.value)
  }
  df <- as.data.frame(p_vec)
  rownames(df) <- colnames(x)
  
  return(rownames(which(df == min(p_vec), arr.ind = T)))
}

most_significant(test_data)

# Отличный вариант

most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
  min_p  <- which(chisq_tests == min(chisq_tests))    
  return(colnames(test_data)[min_p])
}

# Задание 4

iris <- iris
tiris <- t(t(iris[-5]) > sapply(iris[-5], function(col) mean(col)))
summ <- apply(tiris, 1, sum)
tiris <- as.data.frame(tiris)
tiris$summ <- summ
tiris$important_cases <- ifelse(tiris$summ >= 3, 'Yes', 'No') 
iris$important_cases <- factor(tiris$important_cases)

# Хорошее решение

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))
colMeans(iris[,1:4])


# Задание 5

test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(x){
  importance_calc <- function(v1, v2, cap = dim(x)[2]/2){
    ifelse(sum(v1 > v2) > cap, 'Yes', 'No')
  }
  x$important_cases <- factor(apply(x, 1, importance_calc, v2 = colMeans(x)))
  return(x)
}

get_important_cases(test_data)

importance_calc <- function(v1, v2 = colMeans((v1)), cap = dim(v1)[2]/2){
  ifelse(sum(v1 > v2) > cap, 'Yes', 'No')
}
importance_calc(test_data)

ifelse(sum(test_data > colMeans(test_data)) > dim(test_data)[2]/2, 'Yes', 'No')
test_data$imp <- sapply()


#Задание 8

diamonds <- diamonds
ggplot(diamonds, aes(color))+
  geom_bar(position = 'dodge', aes(fill = cut))


# Задание 6

stat_mode <- function(x){
  x <- table(as.factor(x))
  return(as.numeric(rownames(which(x == max(x), arr.ind = T))))
}

v <- c(1, 1, 1, 2, 5, 5, 5)
tb <- table(as.factor(v))
which.max(table(as.factor(v)))

stat_mode(v)

which(table(v) == max(table(v)))
names(which(table(v) == max(table(v))))


# Задание 7

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")

max_resid <- function(x){
  fit <- as.data.frame(chisq.test(table(x))$stdres)
  return(c(as.character(fit[which(fit$Freq == max(fit$Freq)),][[1]]), 
           as.character(fit[which(fit$Freq == max(fit$Freq)),][[2]])))
}

#Варианты получения имен таблицы, соответсвующее максимальному значению ст. остатков

fit <- table(test_data)
index <- which(chisq.test(fit)$stdres == max(chisq.test(fit)$stdres), arr.ind = T)
rownames(fit)[index[1]]
colnames(fit)[index[2]]
fit

max_resid(test_data)



