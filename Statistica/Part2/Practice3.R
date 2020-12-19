library(ggplot2) 
library(ggrepel)
library(psych)

# Задание 1

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust<-  function(test_data, cluster_number){
  dist_test <- dist(test_data)
  fit <- hclust(dist_test)
  test_data$cluster <- factor(cutree(fit, cluster_number))
  return(test_data)
}
test_data <- smart_hclust(test_data, 3)

# Задание 2

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data <- as.data.frame(list(X1 = c(15, 16, 9, 24, 16, 16, 28, 28, 33), X2 = c(13, 8, 20, 22, 21, 19, 26, 32, 30), X3 = c(9, 10, 7, 16, 16, 23, 32, 32, 31), X4 = c(10, 6, 7, 18, 23, 24, 35, 29, 27)))

get_difference <- function(test_data, n_cluster){
  test_data <- smart_hclust(test_data, n_cluster)
  tt <- sapply(test_data[-grep('cluster',colnames(test_data))], function(col) summary(aov(col ~ cluster, test_data))[[1]]$'Pr(>F)'[[1]])
  return(names(tt[tt > 0.05]))
}


# Задание 3

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(d){
  return(cbind(d,prcomp(d)$x[,c(1,2)]))
}

# Задание 4

test_data <- swiss

get_pca2 <- function(data){
  imp <- summary(prcomp(data))$importance[3,]
  var_names <- names(c(imp[imp < 0.9], imp[imp >= 0.9][1]))
  massive <- cbind(data, as.data.frame(prcomp(data)$x)[var_names])
  return(massive)
}

# Задание 5
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- as.data.frame(list(V1 = c(7, 9, 14, 19, 17), 
                                V2 = c(32, 20, 2, 16, 7), 
                                V3 = c(9, 7, 15, 5, 10), 
                                V4 = c(14, 16, 21, 26, 24), 
                                V5 = c(-26, -14, 4, -10, -1)))
test_data <- as.data.frame(list(V1 = c(22, 20, 7, 24, 6), 
                                V2 = c(1, -7, -12, -9, 4), 
                                V3 = c(4, 15, 20, 3, 7), 
                                V4 = c(9, 20, 25, 8, 12), 
                                V5 = c(6, 14, 19, 16, 3), 
                                V6 = c(14, 24, 3, 12, 23), 
                                V7 = c(4, 7, 9, 6, 0), 
                                V8 = c(3, 4, 20, 18, 7)))

test_data <- as.data.frame(list(V1 = c(5, -3, 12, 14, 11), V2 = c(13, 13, 12, 5, 27), V3 = c(10, 4, 20, 12, 9), V4 = c(11, 8, 11, 5, 10), V5 = c(7, 15, 16, 10, 13), V6 = c(3, 9, 8, 15, 7)))

is_multicol <- function(d){
  cormat <- corr.test(d)$r
  diag(cormat) <- 0
  ifelse(is.null(rownames(which(abs(cormat) == 1, arr.ind = T))), 
         return("There is no collinearity in the data"),
         return(rownames(which(abs(cormat) == 1, arr.ind = T))))
}


# Задание 6

df <- swiss
library(ggplot2)

df <- smart_hclust(swiss, 2)
ggplot(df, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = 'lm')
