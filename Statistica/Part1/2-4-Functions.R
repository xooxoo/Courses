library(ggplot2)

my_calc <- function(x, y){
  s <- x + y
  d <- x - y
  return(c(s, d))
}

my_calc(3, 4)

my_calc_2 <- function(x, y, z = 10){
  s = x + y + z
  d = x - y - z
  return(c(s, d))
}

my_calc_2(2, 4, 1)
my_calc_2(2, 4)

dist1 <- rnorm(1000)
dist2 <- runif(1000)
dist1[1:30] <- NA
dist2[1:30] <- NA

dist1[is.na(dist1)] <- mean(dist1, na.rm = T)

my_na_rm <- function(x){
  if (is.numeric(x)){
    stat_test <- shapiro.test(x) #Тест шапиро - это тест на нормальность. Если p-val меньше 0.05, то распределение отклоняется от нормального
    if (stat_test$p.value > 0.05){
      print('mean')
      x[is.na(x)] <- mean(x, na.rm = T)
    }
    else {
      print('median')
      x[is.na(x)] <- median(x, na.rm = T)
    }
    return(x)
  }
  else {
    print('X is not numeric')
  }
}

shapiro.test(dist1)

dist1 <- my_na_rm(x = dist1)
dist2 <- my_na_rm(x = dist2)
hist(dist2)

#Объединение многих файлов в один dataframe
setwd('DataAnalisys/Grants data')
dir(pattern = '*.csv')

grants <- data.frame()
for (i in dir(pattern = '*.csv')){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}

read_data <- function(){
  df <- data.frame()
  for (i in dir(pattern = '*.csv')){
    temp_df <<- read.csv(i)
    df <- rbind(temp_df, df)
  }
  print(paste(length(dir()), 'files were combined'))
  return(df)
}
grants <- read_data()

# задаание 3

filtered.sum <- function(x){
  return(sum(x[x > 0], na.rm = T))
}
filtered.sum(c(-3,0, NA, 1,2))

# Задание 4
dir('/home/stanislav/Документы/Работа/ExpeditionData/CATs/After')
df <- read.csv('/home/stanislav/Документы/Работа/ExpeditionData/CATs/After/AK3_RCM11_SN26_1449m.csv')

iqr <- IQR(df$temperature, na.rm = T)
q <- quantile(df$temperature, probs = c(0.25, 0.75))
subdf <- subset(df, df$temperature >= q[1] - iqr & df$temperature <= q[2] + iqr)

